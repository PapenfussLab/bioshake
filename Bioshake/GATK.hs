{-# LANGUAGE TypeOperators, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}
module Bioshake.GATK(reQual) where


import Bioshake
import Development.Shake
import Development.Shake.FilePath
import System.IO.Temp

data ReQual = ReQual FilePath

reQual = ReQual

instance Pathable a => Pathable (a :-> ReQual) where
  paths ((paths -> [p]) :-> _) = [p <.> "gatk.requal.bam"]

instance Pathable a => IsBam (a :-> ReQual)
instance (Pathable a, IsSorted a) => IsSorted (a :-> ReQual)

instance (Referenced a, Pathable a, IsBam a) => Buildable a ReQual where
  build (ReQual jar) a@(paths -> [input]) [out] =
    liftIO . withSystemTempDirectory "gatk" $ \tmpDir -> do
      let tmpFile = tmpDir </> "recal.grp"
      () <- cmd "java"
        ["-jar", jar]
        ["-T",  "BaseRecalibrator"]
        ["-R", getRef a]
        ["-I", input]
        ["-knownSites", "latest_dbsnp.vcf"]
        ["-o", tmpFile]
      () <- cmd "java"
        ["-jar", jar]
        ["-T",  "PrintReads"]
        ["-R", getRef a]
        ["-I", input]
        ["-BQSR", tmpFile]
        ["-o", out]
      return ()
