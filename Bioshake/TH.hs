{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Bioshake.TH where

import           Bioshake
import           Bioshake.Cluster.Torque  (Config, TOption (CPUs), getCPUs,
                                           submit)
import           Bioshake.Implicit        (Implicit_, param_)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import           Data.Char
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Development.Shake        (Action (..), CmdOption (Shell),
                                           actionFinally, cmd)
import           Language.Haskell.TH
import           System.Directory         (removeDirectoryRecursive)
import           System.IO.Temp           (createTempDirectory)

makeSingleTypes :: Name -> [Name] -> [Name] -> DecsQ
makeSingleTypes ty outtags transtags = do
  let name = nameBase ty
      Just mod = nameModule ty
      lastMod = last $ splitOn "." mod
      outbase = nameBase $ head outtags
      'I':'s':ext' = outbase
      ext = map toLower ext'


  path <- [d| instance Pathable a => Pathable (a :-> $(conT ty) c) where paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> lastMod <.> name <.> ext] |]

  tags <- forM outtags $ \t -> do
    a <- newName "a"
    c <- newName "c"
    return (InstanceD Nothing [AppT (ConT ''Pathable) (VarT a)] (AppT (ConT t) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (VarT c)))) [])

  transtags <- forM transtags $ \t -> do
    a <- newName "a"
    c <- newName "c"
    return (InstanceD Nothing [AppT (ConT ''Pathable) (VarT a), AppT (ConT t) (VarT a)] (AppT (ConT t) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (VarT c)))) [])


  return $ path ++ tags ++ transtags

makeMultiTypes :: Name -> [Name] -> [Name] -> DecsQ
makeMultiTypes ty outtags transtags = do
  let name = nameBase ty
      Just mod = nameModule ty
      lastMod = last $ splitOn "." mod
      outbase = nameBase $ head outtags
      'I':'s':ext' = outbase
      ext = map toLower ext'


  path <- [d| instance Pathable a => Pathable (a :-> $(conT ty) c) where paths (a :-> _) = map (\x -> "tmp" </> takeFileName x <.> lastMod <.> name <.> ext) (paths a) |]

  tags <- forM outtags $ \t -> do
    a <- newName "a"
    c <- newName "c"
    return (InstanceD Nothing [AppT (ConT ''Pathable) (VarT a)] (AppT (ConT t) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (VarT c)))) [])

  transtags <- forM transtags $ \t -> do
    a <- newName "a"
    c <- newName "c"
    return (InstanceD Nothing [AppT (ConT ''Pathable) (VarT a), AppT (ConT t) (VarT a)] (AppT (ConT t) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (VarT c)))) [])


  return $ path ++ tags ++ transtags

makeSingleThread ty tags fun = do
  let name = nameBase ty
      name' = map toLower name

  TyConI (DataD _ _ _ _ [NormalC con _] _) <- reify ty


  consName <- newName name'
  constructor <- return $ ValD (VarP consName) (NormalB (AppE (ConE con) (ConE '()))) []

  a <- newName "a"
  inputs <- newName "inputs"
  out <- newName "out"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : tags
  build <- return $ InstanceD Nothing tags' (AppT (AppT (ConT ''Buildable) (VarT a)) (AppT (ConT ty) (TupleT 0))) [FunD 'build [Clause [VarP a,VarP inputs,VarP out] (NormalB (AppE (VarE 'withCmd) (SigE (AppE (AppE (AppE (VarE fun) (VarE a)) (VarE inputs)) (VarE out)) (AppT (ConT ''Cmd) (TupleT 0))))) []]]

  return [constructor, build]

makeSingleCluster ty tags fun = do
  let name = nameBase ty
      name' = map toLower name

  TyConI (DataD _ _ _ _ [NormalC con conTypes] _) <- reify ty

  let (_, _:conTypes') = unzip conTypes
      conArrTypes = map (\t -> AppT ArrowT t) conTypes'
      funType = foldr (\l r -> AppT l r) (AppT (ConT ty) (ConT ''Config)) conArrTypes

  consName <- newName name'
  constructorSig <- return $ SigD consName (ForallT [] [AppT (ConT ''Implicit_) (ConT ''Config)] funType)
  constructor <- return $ ValD (VarP consName) (NormalB (AppE (ConE con) (VarE 'param_))) []

  a <- newName "a"
  inputs <- newName "inputs"
  out <- newName "out"
  config <- newName "config"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : tags
  build <- return $ InstanceD Nothing tags' (AppT (AppT (ConT ''Buildable) (VarT a)) (AppT (ConT ty) (ConT ''Config))) [FunD 'build [Clause [AsP a (ConP con (VarP config : replicate (length conArrTypes) WildP)),VarP inputs,VarP out] (NormalB (AppE (AppE (VarE 'withSubmit) (SigE (AppE (AppE (AppE (VarE fun) (VarE a)) (VarE inputs)) (VarE out)) (AppT (ConT ''Cmd) (TupleT 0)))) (ListE [AppE (ConE 'Left) (VarE config),AppE (ConE 'Right) (AppE (ConE 'CPUs) (LitE (IntegerL 1)))])) ) []]]

  return [constructorSig, constructor, build]

-- Multithreaded versions of the above

makeThreaded ty tags fun = do
  let name = nameBase ty
      name' = map toLower name

  TyConI (DataD _ _ _ _ [NormalC con conTypes] _) <- reify ty

  let (_, _:conTypes') = unzip conTypes
      conArrTypes = map (\t -> AppT ArrowT t) conTypes'
      funType = foldr (\l r -> AppT l r) (AppT (ConT ty) (ConT ''Threads)) conArrTypes

  consName <- newName name'
  constructorSig <- return $ SigD consName (ForallT [] [AppT (ConT ''Implicit_) (ConT ''Threads)] funType)
  constructor <- return $ ValD (VarP consName) (NormalB (AppE (ConE con) (VarE 'param_))) []

  a <- newName "a"
  b <- newName "b"
  inputs <- newName "inputs"
  out <- newName "out"
  t <- newName "t"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : tags
  build <- return $ InstanceD Nothing tags' (AppT (AppT (ConT ''Buildable) (VarT a)) (AppT (ConT ty) (ConT ''Threads))) [FunD 'build [Clause [AsP a (ConP con (AsP b (ConP 'Threads [VarP t]) : replicate (length conArrTypes) WildP)), VarP inputs,VarP out] (NormalB (AppE (VarE 'withCmd) (SigE (AppE (AppE (AppE (AppE (VarE fun) (VarE t)) (VarE a)) (VarE inputs)) (VarE out)) (AppT (ConT ''Cmd) (TupleT 0))))) []], FunD 'threads [Clause [WildP, AsP a (ConP con (AsP b (ConP 'Threads [VarP t]) : replicate (length conArrTypes) WildP))] (NormalB (VarE t)) []]]

  return [constructorSig, constructor, build]


makeCluster ty tags fun = do
  let name = nameBase ty
      name' = map toLower name

  TyConI (DataD _ _ _ _ [NormalC con conTypes] _) <- reify ty

  let (_, _:conTypes') = unzip conTypes
      conArrTypes = map (\t -> AppT ArrowT t) conTypes'
      funType = foldr (\l r -> AppT l r) (AppT (ConT ty) (ConT ''Config)) conArrTypes

  consName <- newName name'
  constructorSig <- return $ SigD consName (ForallT [] [AppT (ConT ''Implicit_) (ConT ''Config)] funType)
  constructor <- return $ ValD (VarP consName) (NormalB (AppE (ConE con) (VarE 'param_))) []

  a <- newName "a"
  inputs <- newName "inputs"
  out <- newName "out"
  config <- newName "config"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : tags
  build <- return $ InstanceD Nothing tags' (AppT (AppT (ConT ''Buildable) (VarT a)) (AppT (ConT ty) (ConT ''Config))) [FunD 'build [Clause [AsP a (ConP con (VarP config : replicate (length conArrTypes) WildP)),VarP inputs,VarP out] (NormalB (AppE (AppE (VarE 'withSubmit) (SigE (AppE (AppE (AppE (AppE (VarE fun) (AppE (VarE 'getCPUs) (VarE config))) (VarE a)) (VarE inputs)) (VarE out)) (AppT (ConT ''Cmd) (TupleT 0)))) (ListE [AppE (ConE 'Left) (VarE config)]))) []]]

  return [constructorSig, constructor, build]

-- For writing commands succinctly
class Args a where args :: a -> [String]

instance Args String where args = words
instance Args [String] where args = id

class CArgs a where cmdArgs :: [String] -> a

instance CArgs [String] where cmdArgs = id
instance (Args a, CArgs r) => CArgs (a -> r) where cmdArgs a r = cmdArgs $ a ++ args r

instance CArgs (Cmd ()) where cmdArgs r = liftF $ CmdF r ()

type a |-> b = a

data CmdF a where
  CmdF :: [String] -> a -> CmdF a
  FinallyF :: Cmd () -> IO () -> a -> CmdF a
deriving instance Functor CmdF

type Cmd = FreeT CmdF Action

cmdFinally :: Cmd () -> IO () -> Cmd ()
cmdFinally a f = liftF $ FinallyF a f ()

withTempDirectory' :: FilePath -> String -> (FilePath -> Cmd ()) -> Cmd ()
withTempDirectory' targetDir template act = do
  path <- liftIO $ createTempDirectory targetDir template
  act path `cmdFinally` (liftIO . ignoringIOErrors $ removeDirectoryRecursive path)

run :: CArgs a => a |-> Cmd ()
run = cmdArgs []

withCmd :: Cmd () -> Action ()
withCmd x' = do
  x <- runFreeT x'
  case x of
    Pure _ -> return ()
    Free (CmdF str a) -> do
      () <- cmd Shell str
      withCmd a
    Free (FinallyF a f a') -> do
      withCmd a `actionFinally` f
      withCmd a'

withSubmit :: Cmd () -> [Either Config TOption] -> Action ()
withSubmit x' config = do
  x <- runFreeT x'
  case x of
    Pure _ -> return ()
    Free (CmdF str a) -> do
      () <- submit str config
      withSubmit a config
    Free (FinallyF a f a') -> do
      withSubmit a config `actionFinally` f
      withSubmit a' config
