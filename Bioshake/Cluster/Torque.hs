{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, TypeOperators, FlexibleContexts #-}
module Bioshake.Cluster.Torque where

import Control.Monad
import Data.Either
import Data.List
import Development.Shake.FilePath
import Development.Shake hiding (doesFileExist)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.IO.Temp
import System.Posix.Files

type ModuleName = String
type Queue = String

data TOption = Mem Int
             | CPUs Int
             | Walltime Int
             | Queue String
             | Module ModuleName
             | TStdout FilePath
             deriving (Eq, Show)

class TArgs a where cmdArgs :: [Either TOption String] -> a
instance (Args a, TArgs r) => TArgs (a -> r) where cmdArgs a r = cmdArgs $ a ++ args r

instance TArgs (Action ()) where
  cmdArgs args = liftIO . withTempDirectory "." "qsub" $ \tmpDir -> do
      let scriptfile = tmpDir </> "run.sh"
          okfile = tmpDir </> "run.sh.ok"
      writeFile scriptfile $ script okfile
      setFileMode scriptfile 755
      cwd <- getCurrentDirectory
      unit $ cmd "qsub -I" ["-d", cwd] stdout resflags ["-x", scriptfile]
      ok <- doesFileExist okfile
      unless ok . error $ "job failed: " ++ exec
      return ()
    where
      script ok = unlines $ ["#!/bin/sh"]
                            ++ map ("module add "++) modules
                            ++ [unwords [exec, "&& touch ", ok]]

      modules = map unModule $ filter isModule options
      options = lefts args
      isModule (Module _) = True
      isModule _ = False
      unModule (Module a) = a

      isStdout (TStdout _) = True
      isStdout _ = False
      unStdout (TStdout path) = FileStdout path
      stdout = unStdout <$> find isStdout options

      exec = unwords $ rights args
      resflags = map toFlag options
        where
          toFlag (Mem n) = "-l mem=" ++ show n
          toFlag (CPUs n) = "-l nodes=1:ppn=" ++ show n
          toFlag (Queue name) = "-q " ++ name
          toFlag _ = []

class Args a where args :: a -> [Either TOption String]
instance Args String where args = map Right . words
instance Args [String] where args = map Right
instance Args TOption where args = return . Left

type a |-> b = a

submit :: TArgs a => a |-> Action ()
submit = cmdArgs []

gb :: Num a => a -> a
gb = (*1024)

