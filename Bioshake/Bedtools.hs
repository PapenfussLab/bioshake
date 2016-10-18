{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Bedtools(bam2bed, bam2bedpe) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Data.Maybe
import System.IO.Temp

data BAM2BED = BAM2BED FilePath
data BAM2BEDpe = BAM2BEDpe FilePath

instance Pathable a => Pathable (a :-> BAM2BED) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "bamtools.bed"]

instance Pathable a => Pathable (a :-> BAM2BEDpe) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "bamtools.bedpe"]

bam2bed = BAM2BED
bam2bedpe = BAM2BEDpe

instance IsBam a => Buildable a BAM2BED where
  build params (paths -> [input]) [out] =
    cmd "bedtools" ["-i", input] (FileStdout out)

instance (IsPairedEnd a, IsBam a) => Buildable a BAM2BEDpe where
  build params (paths -> [input]) [out] =
    cmd "bedtools" ["-i", input] "-bedpe" (FileStdout out)
