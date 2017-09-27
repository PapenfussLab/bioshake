{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


-- | Bioshake is built on a lot of instantiation and template haskell can be
-- used to derive instances for the most common types of processes. In
-- particular, this module provides TH functions for handling either single or
-- multithreaded processes with single or multiple outputs (of the same type).
-- Most things fit into these categories.
module Bioshake.TH where

import           Bioshake
import           Bioshake.Cluster.Torque    (Config, TOption (CPUs, Mem),
                                             getCPUs, submit)
import           Bioshake.Implicit          (Implicit, param)
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

-- | Generate instances of 'Pathable' and for all the tags associated with the
-- output (both transitive "Bioshake.Tags" and absolute "Bioshake.Tags") for
-- actions that produce a single output file. Transitive "Bioshake.Tags" are
-- those that if they exist on the input then they will exist on the output
-- (e.g., a 'Sorted' input may result in a 'Sorted' output because the action
-- does not reorder things). Absolute "Bioshake.Tags" are those that hold on all
-- outputs of the action. These include the file type (e.g., 'IsBam').
makeSingleTypes :: Name -- ^ The type to generate instances for
                -> [Name] -- ^ A list of absolute "Bioshake.Tags". The first of
                          -- these must be of the form IsEXT as it is used to
                          -- generate the extension of the output. Examples
                          -- include 'IsVCF', 'IsBam', and 'IsSam'. The Is part
                          -- is stripped and the lower case remainder used for
                          -- the extension. These tags will hold on outputs
                          -- produced by this action.
                -> [Name] -- ^ Transitive tags. If these hold on the input then
                          -- they transit to the output.
                -> DecsQ
makeSingleTypes ty outtags transtags = do
  let name = nameBase ty
      Just mod = nameModule ty
      lastMod = last $ splitOn "." mod
      outbase = nameBase $ head outtags
      'I':'s':ext' = outbase
      ext = map toLower ext'


  path <- [d| instance (Show c, Pathable a) => Pathable (a :-> $(conT ty) c) where paths (a :-> b) = [hashPath (paths a, show b) <.> lastMod <.> name <.> ext] |]

  tags <- forM outtags $ \t -> do
    a <- newName "a"
    c <- newName "c"
    return (InstanceD Nothing [] (AppT (ConT t) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (VarT c)))) [])

  transtags <- forM transtags $ \t -> do
    a <- newName "a"
    c <- newName "c"
    return (InstanceD Nothing [AppT (ConT t) (VarT a)] (AppT (ConT t) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (VarT c)))) [])


  return $ path ++ tags ++ transtags

-- | Same as 'makeSingleTypes' but for actions that produce an output file for
-- each input file.
makeMultiTypes :: Name -- ^ The type to generate instances for
               -> [Name] -- ^ A list of absolute "Bioshake.Tags". The first of
                         -- these must be of the form IsEXT as it is used to
                         -- generate the extension of the output. Examples
                         -- include 'IsVCF', 'IsBam', and 'IsSam'. The Is part
                         -- is stripped and the lower case remainder used for
                         -- the extension. These tags will hold on outputs
                         -- produced by this action.
               -> [Name] -- ^ Transitive tags. If these hold on the input then
                         -- they transit to the output.
               -> DecsQ
makeMultiTypes ty outtags transtags = do
  let name = nameBase ty
      Just mod = nameModule ty
      lastMod = last $ splitOn "." mod
      outbase = nameBase $ head outtags
      'I':'s':ext' = outbase
      ext = map toLower ext'


  path <- [d| instance (Show c, Pathable a) => Pathable (a :-> $(conT ty) c) where paths (a :-> b) = map (\x -> hashPath (x, show b) <.> lastMod <.> name <.> ext) (paths a) |]

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
  build <- makeSingleThread' ty tags fun
  return (constructor:build)

makeSingleThread' ty tags fun = do
  a <- newName "a"
  a2 <- newName "a2"
  b <- newName "b"
  pipe <- newName "pipe"
  outs <- newName "outs"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : tags
  build <- return $ InstanceD Nothing tags' (AppT (ConT ''Buildable) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (TupleT 0)))) [FunD 'build [Clause [AsP pipe (InfixP (VarP a2) '(:->) (VarP b))] (NormalB (LetE [ValD (VarP outs) (NormalB (AppE (VarE 'paths) (VarE pipe))) []] (AppE (AppE (VarE 'withCmd) (LitE (IntegerL 1))) (AppE (AppE (AppE (VarE fun) (VarE b)) (VarE a2)) (VarE outs))))) []]]

  return [build]

makeSingleCluster ty tags fun = do
  let (c : name) = nameBase ty
      name' = toLower c : name

  TyConI (DataD _ _ _ _ [NormalC con conTypes] _) <- reify ty

  let (_, _:conTypes') = unzip conTypes
      conArrTypes = map (\t -> AppT ArrowT t) conTypes'
      funType = foldr (\l r -> AppT l r) (AppT (ConT ty) (ConT ''Config)) conArrTypes

  consName <- newName name'
  constructorSig <- return $ SigD consName (ForallT [] [AppT (ConT ''Implicit) (ConT ''Config)] funType)
  constructor <- return $ ValD (VarP consName) (NormalB (AppE (ConE con) (VarE 'param))) []

  a <- newName "a"
  a2 <- newName "a2"
  outs <- newName "out"
  pipe <- newName "pipe"
  config <- newName "config"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : tags
  build <- return $ InstanceD Nothing tags' (AppT (ConT ''Buildable) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (ConT ''Config)))) [FunD 'build [Clause [AsP pipe (InfixP (VarP a2) '(:->) (AsP a (ConP con (VarP config : replicate (length conArrTypes) WildP))))] (NormalB (LetE [ValD (VarP outs) (NormalB (AppE (VarE 'paths) (VarE pipe))) []] (AppE (AppE (VarE 'withSubmit) (SigE (AppE (AppE (AppE (VarE fun)  (VarE a)) (VarE a2)) (VarE outs)) (AppT (ConT ''Cmd) (TupleT 0)))) (ListE [AppE (ConE 'Left) (VarE config),AppE (ConE 'Right) (AppE (ConE 'CPUs) (LitE (IntegerL 1)))])))) []]]

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
  constructorSig <- return $ SigD consName (ForallT [] [AppT (ConT ''Implicit) (ConT ''Threads)] funType)
  constructor <- return $ ValD (VarP consName) (NormalB (AppE (ConE con) (VarE 'param))) []
  build <- makeThreaded' ty tags fun
  return (constructorSig : constructor : build)

makeThreaded' ty tags fun = do
  TyConI (DataD _ _ _ _ [NormalC con conTypes] _) <- reify ty
  let (_, _:conTypes') = unzip conTypes
      conArrTypes = map (\t -> AppT ArrowT t) conTypes'
      funType = foldr (\l r -> AppT l r) (AppT (ConT ty) (ConT ''Threads)) conArrTypes

  a <- newName "a"
  a2 <- newName "a2"
  b <- newName "b"
  outs <- newName "outs"
  pipe <- newName "pipe"
  t <- newName "t"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : ''Show : tags
  build <- return $ InstanceD Nothing tags' (AppT (ConT ''Buildable) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (ConT ''Threads)))) [FunD 'build [Clause [AsP pipe (InfixP (VarP a2) '(:->) (AsP b (ConP con (ConP 'Threads [VarP t] : replicate (length conArrTypes) WildP))))] (NormalB (LetE [ValD (VarP outs) (NormalB (AppE (VarE 'paths) (VarE pipe))) []] (AppE (AppE (VarE 'withCmd) (VarE t)) (AppE (AppE (AppE (AppE (VarE fun) (VarE t)) (VarE b)) (VarE a2)) (VarE outs))))) []]]

  return [build]


makeCluster ty tags fun = do
  let (c : name) = nameBase ty
      name' = toLower c : name

  TyConI (DataD _ _ _ _ [NormalC con conTypes] _) <- reify ty

  let (_, _:conTypes') = unzip conTypes
      conArrTypes = map (\t -> AppT ArrowT t) conTypes'
      funType = foldr (\l r -> AppT l r) (AppT (ConT ty) (ConT ''Config)) conArrTypes

  consName <- newName name'
  constructorSig <- return $ SigD consName (ForallT [] [AppT (ConT ''Implicit) (ConT ''Config)] funType)
  constructor <- return $ ValD (VarP consName) (NormalB (AppE (ConE con) (VarE 'param))) []
  build <- makeCluster' ty tags fun
  return (constructorSig : constructor : build)

makeCluster' ty tags fun = do
  TyConI (DataD _ _ _ _ [NormalC con conTypes] _) <- reify ty

  let (_, _:conTypes') = unzip conTypes
      conArrTypes = map (\t -> AppT ArrowT t) conTypes'
      funType = foldr (\l r -> AppT l r) (AppT (ConT ty) (ConT ''Config)) conArrTypes

  a <- newName "a"
  a2 <- newName "a2"
  pipe <- newName "pipe"
  outs <- newName "outs"
  config <- newName "config"
  let tags' = map (\t -> AppT (ConT t) (VarT a)) $ ''Pathable : ''Show : tags
  build <- return $ InstanceD Nothing tags' (AppT (ConT ''Buildable) (AppT (AppT (ConT ''(:->)) (VarT a)) (AppT (ConT ty) (ConT ''Config)))) [FunD 'build [Clause [AsP pipe (InfixP (VarP a2) '(:->) (AsP a (ConP con (VarP config : replicate (length conArrTypes) WildP))))] (NormalB (LetE [ValD (VarP outs) (NormalB (AppE (VarE 'paths) (VarE pipe))) []] (AppE (AppE (VarE 'withSubmit) (SigE (AppE (AppE (AppE (AppE (VarE fun) (AppE (VarE 'getCPUs) (VarE config))) (VarE a)) (VarE a2)) (VarE outs)) (AppT (ConT ''Cmd) (TupleT 0)))) (ListE [AppE (ConE 'Left) (VarE config)])))) []]]

  return [build]

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

withCmd :: Implicit Resource => Int -> Cmd () -> Action ()
withCmd t x' = do
    x <- runFreeT x'
    case x of
        Pure _ -> return ()
        Free (CmdF str a) -> do
            () <- withResource param t $ cmd Shell str
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
