{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Gridss(call, toBEDpe, Call(..)) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Data.Maybe
import System.IO.Temp

data Call = Call { reference :: FilePath
                 , jar :: FilePath
                 , threads :: Int
                 , resource :: Maybe Resource}

data ToBEDpe = ToBEDpe FilePath

instance Pathable a => Pathable (a :-> Call) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "gridss.vcf"]

instance Pathable a => Pathable (a :-> ToBEDpe) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "gridss" <.> status <.> "bedpe" | status <- ["unfilt", "filt"]]

call ref jar = Call ref jar 0 Nothing
toBEDpe = ToBEDpe

instance (IsSorted a, IsBam a) => Buildable a Call where
  build params (paths -> [input]) [out] =
    let cmd' =
          liftIO . withSystemTempDirectory "gridss" $ \tmpDir ->
            cmd "java -ea -Xmx16g"
              ["-cp", jar params, "au.edu.wehi.idsv.Idsv"]
              ["TMP_DIR=", tmpDir]
              ["WORKING_DIR=", tmpDir]
              ["REFERENCE=", reference params]
              ["INPUT=", input]
              ["IC=1"]
              ["OUTPUT=", out]
              ["WORKER_THREADS=", show (threads params)]
    in case resource params of
         Just res -> withResource res (threads params) cmd'
         Nothing -> cmd'

instance Pathable a => IsVCF (a :-> Call)

instance IsVCF a => Buildable a ToBEDpe where
  build (ToBEDpe jar) (paths -> [input]) [outUnfilt, outFilt] =
    cmd "java -ea -Xmx16g"
      ["-cp", jar, "au.edu.wehi.idsv.Idsv.VcfBreakendToBedpe"]
      ["INPUT=" ++ input]
      ["OUTPUT=" ++ outUnfilt]
      ["OUTPUT_FILTERED=" ++ outFilt]
