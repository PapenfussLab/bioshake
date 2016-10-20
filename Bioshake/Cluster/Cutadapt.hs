{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Cluster.Cutadapt(trim) where

import Bioshake
import Bioshake.Internal.Cutadapt
import Development.Shake
import Bioshake.Cluster.Torque
import Bioshake.Implicit

trim :: Implicit_ Config => Seq -> Trim Config
trim = Trim param_

instance IsFastQ a => Buildable a (Trim Config) where
  build (Trim config three') (paths -> [input]) [out] =
    submit "cutadapt"
      ["-a", show three']
      ["-o", out]
      [input]
      config
      (CPUs 1)
  build (Trim config three') (paths -> inputs@[_, _]) [out1, out2] =
    submit "cutadapt"
      ["-a", show three']
      ["-o", out1]
      ["-p", out2]
      inputs
      config
      (CPUs 1)
