{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Cutadapt(trim) where

import Bioshake
import Bioshake.Internal.Cutadapt
import Development.Shake
import Development.Shake.FilePath

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
