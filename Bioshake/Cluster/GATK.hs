{-# LANGUAGE TypeOperators, ViewPatterns, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Bioshake.Cluster.GATK(reQual) where

import Bioshake
import Bioshake.Cluster.Torque
import Bioshake.Internal.GATK
import Development.Shake
import Development.Shake.FilePath
import System.IO.Temp
import Data.Implicit

instance (Implicit_ Config, Referenced a, Pathable a, IsBam a) => Buildable a ReQual where
  build (ReQual jar) a@(paths -> [input]) [out] =
    liftIO . withSystemTempDirectory "gatk" $ \tmpDir -> do
      let tmpFile = tmpDir </> "recal.grp"
      () <- submit "java"
        ["-jar", jar]
        ["-T",  "BaseRecalibrator"]
        ["-R", getRef a]
        ["-I", input]
        ["-knownSites", "latest_dbsnp.vcf"]
        ["-o", tmpFile]
        (param_ :: Config)
      () <- submit "java"
        ["-jar", jar]
        ["-T",  "PrintReads"]
        ["-R", getRef a]
        ["-I", input]
        ["-BQSR", tmpFile]
        ["-o", out]
        (param_ :: Config)
      return ()
