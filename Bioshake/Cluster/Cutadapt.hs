{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Cluster.Cutadapt(trim) where

import Bioshake
import Bioshake.Cluster.Torque
import Bioshake.Internal.Cutadapt
import Development.Shake
import Development.Shake.FilePath
import Data.Implicit

instance (Implicit_ Config, IsFastQ a) => Buildable a Trim where
  build (Trim three') (paths -> [input]) [out] =
    submit "cutadapt"
      ["-a", show three']
      ["-o", out]
      [input]
      (param_ :: Config)
  build (Trim three') (paths -> inputs@[_, _]) [out1, out2] =
    submit "cutadapt"
      ["-a", show three']
      ["-o", out1]
      ["-p", out2]
      inputs
      (param_ :: Config)
