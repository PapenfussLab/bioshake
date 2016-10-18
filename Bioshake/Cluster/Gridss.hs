{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Cluster.Gridss(call, toBEDpe, Call(..)) where

import Bioshake
import Bioshake.Cluster.Torque
import Bioshake.Internal.Gridss
import Development.Shake
import Development.Shake.FilePath
import Data.Maybe
import System.IO.Temp
import Data.Implicit

instance (Implicit_ Config, Referenced a, IsSorted a, IsBam a) => Buildable a Call where
  build params a@(paths -> [input]) [out] =
    liftIO . withSystemTempDirectory "gridss" $ \tmpDir ->
      submit "java -ea -Xmx16g"
        ["-cp", jar params, "au.edu.wehi.idsv.Idsv"]
        ["TMP_DIR=", tmpDir]
        ["WORKING_DIR=", tmpDir]
        ["REFERENCE=", getRef a]
        ["INPUT=", input]
        ["IC=1"]
        ["OUTPUT=", out]
        ["WORKER_THREADS=", show (threads params)]
        (param_ :: Config)
        (CPUs (threads params))

instance (IsPairedEnd a, IsVCF a) => Buildable a ToBEDpe where
  build (ToBEDpe jar) (paths -> [input]) [outUnfilt, outFilt] =
    submit "java -ea -Xmx16g"
      ["-cp", jar, "au.edu.wehi.idsv.Idsv.VcfBreakendToBedpe"]
      ["INPUT=" ++ input]
      ["OUTPUT=" ++ outUnfilt]
      ["OUTPUT_FILTERED=" ++ outFilt]
