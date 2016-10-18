{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Cluster.Bedtools(bam2bed, bam2bedpe) where

import Bioshake
import Bioshake.Cluster.Torque
import Bioshake.Internal.Bedtools
import Development.Shake
import Data.Implicit

instance (Implicit_ Config, IsBam a) => Buildable a BAM2BED where
  build params (paths -> [input]) [out] =
    submit "bedtools" ["-i", input] [">", out] (param_ :: Config)

instance (Implicit_ Config, IsPairedEnd a, IsBam a) => Buildable a BAM2BEDpe where
  build params (paths -> [input]) [out] =
    submit "bedtools" ["-i", input] "-bedpe" [">", out] (param_ :: Config)
