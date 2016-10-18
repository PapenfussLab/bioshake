{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Gridss(call, toBEDpe, Call(..)) where

import Bioshake
import Bioshake.Internal.Gridss
import Development.Shake
import Development.Shake.FilePath
import Data.Maybe
import System.IO.Temp

instance (Referenced a, IsSorted a, IsBam a) => Buildable a Call where
  build params a@(paths -> [input]) [out] =
    let mem = threads params * 2 + 8
        cmd' =
          liftIO . withSystemTempDirectory "gridss" $ \tmpDir ->
            cmd "java -ea" (concat ["-Xmx", show mem, "g"])
              ["-cp", jar params, "au.edu.wehi.idsv.Idsv"]
              ["TMP_DIR=", tmpDir]
              ["WORKING_DIR=", tmpDir]
              ["REFERENCE=", getRef a]
              ["INPUT=", input]
              ["IC=1"]
              ["OUTPUT=", out]
              ["WORKER_THREADS=", show (threads params)]
    in case resource params of
         Just res -> withResource res (threads params) cmd'
         Nothing -> cmd'

instance (IsPairedEnd a, IsVCF a) => Buildable a ToBEDpe where
  build (ToBEDpe jar) (paths -> [input]) [outUnfilt, outFilt] =
    cmd "java -ea -Xmx10g"
      ["-cp", jar, "au.edu.wehi.idsv.Idsv.VcfBreakendToBedpe"]
      ["INPUT=" ++ input]
      ["OUTPUT=" ++ outUnfilt]
      ["OUTPUT_FILTERED=" ++ outFilt]
