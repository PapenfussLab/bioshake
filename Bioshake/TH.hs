{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Bioshake.TH where

import           Bioshake
import           Bioshake.Cluster.Torque    (Config, TOption (CPUs, Mem),
                                             getCPUs, submit)
import           Bioshake.Implicit          (Implicit_, param_)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import qualified Crypto.Hash                as H
import           Data.Binary                (Binary, encode)
import qualified Data.ByteString            as B
import           Data.ByteString.Lazy       (toStrict)
import           Data.Char
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Development.Shake          (Action (..), CmdOption (Shell),
                                             Resource, actionFinally, cmd, unit,
                                             withResource)
import           Development.Shake.FilePath
import           Language.Haskell.TH
import           System.Directory           (removeDirectoryRecursive)
import           System.IO.Temp             (createTempDirectory)
import           System.Posix.Files         (createLink, rename)

sha1 :: B.ByteString -> H.Digest H.SHA1
sha1 = H.hash

hashPath :: Binary b => b -> FilePath
hashPath = ("tmp" </> ) . show . sha1 . toStrict . encode

makeSingleTypes :: Name -> [Name] -> [Name] -> DecsQ
makeSingleTypes ty outtags transtags = do
  let name = nameBase ty
      Just mod = nameModule ty
      lastMod = last $ splitOn "." mod
      outbase = nameBase $ head outtags
      'I':'s':ext' = outbase
      ext = map toLower ext'


  path <- [d| instance (Show a, Pathable a) => Pathable (a :-> $(conT ty) c) where paths (a :-> _) = [hashPath (paths a, show a) <.> lastMod <.> name <.> ext] |]

  tags <- forM outtags $ \t -> do
    a <- newName "a"
    c <- newName "c"
    return (InstanceD Nothing [] (AppT (ConT t) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (VarT c)))) [])

  transtags <- forM transtags $ \t -> do
    a <- newName "a"
    c <- newName "c"
    return (InstanceD Nothing [AppT (ConT t) (VarT a)] (AppT (ConT t) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (VarT c)))) [])


  return $ path ++ tags ++ transtags

makeMultiTypes :: Name -> [Name] -> [Name] -> DecsQ
makeMultiTypes ty outtags transtags = do
  let name = nameBase ty
      Just mod = nameModule ty
      lastMod = last $ splitOn "." mod
      outbase = nameBase $ head outtags
      'I':'s':ext' = outbase
      ext = map toLower ext'


  path <- [d| instance Pathable a => Pathable (a :-> $(conT ty) c) where paths (a :-> _) = map (\x -> hashPath x <.> lastMod <.> name <.> ext) (paths a) |]

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
  let (c : name) = nameBase ty
      name' = toLower c : name

  TyConI (DataD _ _ _ _ [NormalC con _] _) <- reify ty


  consName <- newName name'
  constructor <- return $ ValD (VarP consName) (NormalB (AppE (ConE con) (ConE '()))) []

  a <- newName "a"
  a2 <- newName "a2"
  b <- newName "b"
  pipe <- newName "pipe"
  outs <- newName "outs"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : ''Show : tags
  build <- return $ InstanceD Nothing tags' (AppT (ConT ''Buildable) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (TupleT 0)))) [FunD 'build [Clause [AsP pipe (InfixP (VarP a2) '(:->) (VarP b))] (NormalB (LetE [ValD (VarP outs) (NormalB (AppE (VarE 'paths) (VarE pipe))) []] (AppE (AppE (VarE 'withCmd) (LitE (IntegerL 1))) (AppE (AppE (AppE (VarE fun) (VarE b)) (VarE a2)) (VarE outs))))) []]]

  return [constructor, build]

makeSingleCluster ty tags fun = do
  let (c : name) = nameBase ty
      name' = toLower c : name

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
  let (c : name) = nameBase ty
      name' = toLower c : name

  TyConI (DataD _ _ _ _ [NormalC con conTypes] _) <- reify ty

  let (_, _:conTypes') = unzip conTypes
      conArrTypes = map (\t -> AppT ArrowT t) conTypes'
      funType = foldr (\l r -> AppT l r) (AppT (ConT ty) (ConT ''Threads)) conArrTypes

  consName <- newName name'
  constructorSig <- return $ SigD consName (ForallT [] [AppT (ConT ''Implicit_) (ConT ''Threads)] funType)
  constructor <- return $ ValD (VarP consName) (NormalB (AppE (ConE con) (VarE 'param_))) []

  a <- newName "a"
  a2 <- newName "a2"
  b <- newName "b"
  inputs <- newName "inputs"
  outs <- newName "outs"
  pipe <- newName "pipe"
  t <- newName "t"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : ''Show : tags
  build <- return $ InstanceD Nothing tags' (AppT (ConT ''Buildable) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (ConT ''Threads)))) [FunD 'build [Clause [AsP pipe (InfixP (VarP a2) '(:->) (AsP b (ConP con (ConP 'Threads [VarP t] : replicate (length conArrTypes) WildP))))] (NormalB (LetE [ValD (VarP outs) (NormalB (AppE (VarE 'paths) (VarE pipe))) []] (AppE (AppE (VarE 'withCmd) (VarE t)) (AppE (AppE (AppE (AppE (VarE fun) (VarE t)) (VarE b)) (VarE a2)) (VarE outs))))) []]]

  return [constructorSig, constructor, build]


makeCluster ty tags fun = do
  let (c : name) = nameBase ty
      name' = toLower c : name

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
  MemLimit :: Int -> Cmd () -> a -> CmdF a
  FinallyF :: Cmd () -> IO () -> a -> CmdF a
deriving instance Functor CmdF

type Cmd = FreeT CmdF Action

cmdFinally :: Cmd () -> IO () -> Cmd ()
cmdFinally a f = liftF $ FinallyF a f ()

memLimit :: Int -> Cmd () -> Cmd ()
memLimit a f = liftF $ MemLimit a f ()

withTempDirectory' :: FilePath -> String -> (FilePath -> Cmd ()) -> Cmd ()
withTempDirectory' targetDir template act = do
  path <- liftIO $ createTempDirectory targetDir template
  act path `cmdFinally` (liftIO . ignoringIOErrors $ removeDirectoryRecursive path)

run :: CArgs a => a |-> Cmd ()
run = cmdArgs []

withCmd :: Implicit_ Resource => Int -> Cmd () -> Action ()
withCmd t x' = do
    x <- runFreeT x'
    case x of
        Pure _ -> return ()
        Free (CmdF str a) -> do
            () <- withResource param_ t $ cmd Shell str
            withCmd t a
        Free (FinallyF a f a') -> do
            withCmd t a `actionFinally` f
            withCmd t a'
        Free (MemLimit _ a b) -> do
          withCmd t a
          withCmd t b

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
    Free (MemLimit mem b a) -> do
      cmd <- runFreeT b
      case cmd of
        Free (CmdF str a) -> unit $ submit str config (Mem mem)
        x -> error "Can only memory limit run commands"
      withSubmit a config
