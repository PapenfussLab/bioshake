{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Bedtools(bam2bed, bam2bedpe) where

import Bioshake
import Bioshake.Internal.Bedtools
import Development.Shake

instance IsBam a => Buildable a BAM2BED where
  build params (paths -> [input]) [out] =
    cmd "bedtools" ["-i", input] (FileStdout out)

instance (IsPairedEnd a, IsBam a) => Buildable a BAM2BEDpe where
  build params (paths -> [input]) [out] =
    cmd "bedtools" ["-i", input] "-bedpe" (FileStdout out)
