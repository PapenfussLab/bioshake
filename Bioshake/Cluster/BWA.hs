{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Cluster.BWA(align, Align(..)) where

import Bioshake
import Bioshake.Internal.BWA
import Development.Shake
import Bioshake.Implicit
import Bioshake.Cluster.Torque

align :: Implicit_ Config => Align Config
align = Align param_

instance (Referenced a, IsFastQ a) => Buildable a (Align Config) where
  build (Align config) a@(paths -> inputs) [out] =
    submit "bwa mem"
      ["-t", show $ getCPUs config]
      [getRef a]
      inputs
      [">", out]
      config
