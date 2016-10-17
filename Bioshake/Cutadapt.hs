{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Cutadapt(trim) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath

data Trim = Trim Seq

trim = Trim

instance Pathable a => Pathable (a :-> Trim) where
  paths (a :-> _) = map (\p -> "tmp" </> takeFileName p <.> "trimmed.fastq.gz") $ paths a

instance Pathable a => IsFastQ (a :-> Trim)

instance IsFastQ a => Buildable a Trim where
  build (Trim three') (paths -> [input]) [out] =
    cmd "cutadapt"
      ["-a", show three']
      ["-o", out]
      [input]
  build (Trim three') (paths -> inputs@[_, _]) [out1, out2] =
    cmd "cutadapt"
      ["-a", show three']
      ["-o", out1]
      ["-p", out2]
      inputs
