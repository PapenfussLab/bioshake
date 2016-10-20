{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Gridss(call, toBEDpe, Call(..)) where

import Bioshake
import Bioshake.Internal.Gridss
import Development.Shake
import System.IO.Temp
import Bioshake.Implicit

call :: Implicit_ Threads => FilePath -> Call Threads
call = Call param_

toBEDpe :: FilePath -> ToBEDpe ()
toBEDpe = ToBEDpe ()

instance (Referenced a, IsSorted a, IsBam a) => Buildable a (Call Threads) where
  threads _ (Call (Threads t) _) = t
  build (Call (Threads t) jar) a@(paths -> inputs) [out] =
    let mem = t * 2 + 8 in
    liftIO . withSystemTempDirectory "gridss" $ \tmpDir ->
      cmd "java -ea" (concat ["-Xmx", show mem, "g"])
        ["-cp", jar, "au.edu.wehi.idsv.Idsv"]
        ["TMP_DIR=", tmpDir]
        ["WORKING_DIR=", tmpDir]
        ["REFERENCE=", getRef a]
        (concat $ zipWith (\input ic -> ["INPUT=", input, "IC=", show ic]) inputs [1..])
        ["OUTPUT=", out]
        ["WORKER_THREADS=", show t]

instance (IsPairedEnd a, IsVCF a) => Buildable a (ToBEDpe ()) where
  build (ToBEDpe _ jar) (paths -> [input]) [outUnfilt, outFilt] =
    cmd "java -ea -Xmx10g"
      ["-cp", jar, "au.edu.wehi.idsv.VcfBreakendToBedpe"]
      ["INPUT=" ++ input]
      ["OUTPUT=" ++ outUnfilt]
      ["OUTPUT_FILTERED=" ++ outFilt]
