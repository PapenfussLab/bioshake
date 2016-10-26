{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Bioshake.Cluster.Torque where

import           Bioshake.Implicit
import           Control.Monad
import           Data.Either
import           Data.List
import qualified Data.Map                   as M
import           Data.Maybe
import           Development.Shake          hiding (doesFileExist)
import           Development.Shake.FilePath
import           System.Directory           (copyFile, doesFileExist,
                                             getCurrentDirectory)
import           System.IO.Temp
import           System.Posix.Files

type ModuleName = String
type Queue = String

data TOption = Mem Int
             | CPUs Int
             | Walltime Int
             | Queue String
             | Module ModuleName
             | TStdout FilePath
             deriving (Eq, Show)

newtype Config = Config [TOption]
--instance Default Config where def = Config [Queue "large", Mem (gb 10), CPUs 1]

getCPUs :: Config -> Int
getCPUs (Config ts) = foldl getCPUs' 1 ts
  where
    getCPUs' _ (CPUs n) = n
    getCPUs' n _        = n

class TArgs a where cmdArgs :: [Either TOption String] -> a
instance (Args a, TArgs r) => TArgs (a -> r) where cmdArgs a r = cmdArgs $ a ++ args r

instance TArgs (Action ()) where cmdArgs = liftIO . cmdArgs
instance TArgs (IO ()) where
  cmdArgs args = withTempDirectory "." "qsub" $ \tmpDir -> do
      let scriptfile = tmpDir </> "run.sh"
          okfile = tmpDir </> "run.sh.ok"
      writeFile scriptfile $ script okfile
      setFileMode scriptfile 0o755
      cwd <- getCurrentDirectory
      unit $ cmd Shell "qsub -I -d" [cwd] stdout resflags "-x" [scriptfile]
      ok <- doesFileExist okfile
      unless ok . error $ "job failed: " ++ exec
      return ()
    where
      script ok = unlines $ ["#!/bin/sh", "set -e"]
                            ++ map ("module add "++) modules
                            ++ [unwords [exec, "&& touch ", ok]]

      modules = map unModule $ filter isModule options
      options = lefts args
      isModule (Module _) = True
      isModule _          = False
      unModule (Module a) = a

      isStdout (TStdout _) = True
      isStdout _           = False
      unStdout (TStdout path) = FileStdout path
      stdout = unStdout <$> find isStdout options

      exec = unwords $ rights args
      dedupOpts = M.elems . M.fromList $ mapMaybe key options
        where
          key a@(Mem _)   = Just ("mem", a)
          key a@(CPUs _)  = Just ("cpus", a)
          key a@(Queue _) = Just ("queue", a)
          key _           = Nothing
      resflags = unwords $ map toFlag dedupOpts
        where
          toFlag (Mem n)      = "-l mem=" ++ show n
          toFlag (CPUs n)     = "-l nodes=1:ppn=" ++ show n
          toFlag (Queue name) = "-q " ++ name
          toFlag _            = []

class Args a where args :: a -> [Either TOption String]
instance Args String where args = map Right . words
instance Args [String] where args = map Right
instance Args TOption where args = return . Left
instance Args Config where args (Config ls) = map Left ls

type a |-> b = a

submit :: TArgs a => a |-> Action ()
submit = cmdArgs []

gb :: Num a => a -> a
gb = (*1024)

