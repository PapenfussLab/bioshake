{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Cluster.Gridss(call, toBEDpe, Call(..)) where

import Bioshake
import Bioshake.Internal.Gridss
import Development.Shake
import Development.Shake.FilePath
import System.IO.Temp
import Bioshake.Implicit
import Bioshake.Cluster.Torque
import System.Directory
import Control.Monad

call :: Implicit_ Config => FilePath -> Call Config
call = Call param_

toBEDpe :: Implicit_ Config => FilePath -> ToBEDpe Config
toBEDpe = ToBEDpe param_

instance (Referenced a, IsSorted a, IsBam a) => Buildable a (Call Config) where
  build (Call config jar) a@(paths -> inputs) [out] =
    let mem = t * 2 + 8
        t = getCPUs config
     in
   liftIO . withTempDirectory "." "gridss" $ \tmpDir -> do
      let inputs' = [tmpDir </> show i <.> "bam" | i <- [1..length inputs]]
          out' = tmpDir </> "out.bam"
      zipWithM_ copyFile inputs inputs'
      () <- submit "java -ea" (concat ["-Xmx", show mem, "g"])
        ["-cp", jar, "au.edu.wehi.idsv.Idsv"]
        ["TMP_DIR=", tmpDir]
        ["WORKING_DIR=", tmpDir]
        ["REFERENCE=", getRef a]
        (concat $ zipWith (\input ic -> ["INPUT=", input, "IC=", show ic]) inputs' [1..])
        ["OUTPUT=", out']
        ["WORKER_THREADS=", show t]
        config
        (Mem (gb mem))
      copyFile out' out

instance (IsPairedEnd a, IsVCF a) => Buildable a (ToBEDpe Config) where
  build (ToBEDpe config jar) (paths -> [input]) [outUnfilt, outFilt] =
    submit "java -ea -Xmx10g"
      ["-cp", jar, "au.edu.wehi.idsv.VcfBreakendToBedpe"]
      ["INPUT=" ++ input]
      ["OUTPUT=" ++ outUnfilt]
      ["OUTPUT_FILTERED=" ++ outFilt]
      config
      (CPUs 1)
