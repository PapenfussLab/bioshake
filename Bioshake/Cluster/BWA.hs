{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Cluster.BWA(align, Align(..)) where

import Bioshake
import Bioshake.Cluster.Torque
import Bioshake.Internal.BWA
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import Data.Implicit

instance (Implicit_ Config, Referenced a, IsFastQ a) => Buildable a Align where
  build params a@(paths -> inputs) [out] =
    submit "bwa mem"
      ["-t", show (threads params)]
      [getRef a]
      inputs
      [">", out]
      (param_ :: Config)
      (CPUs (threads params))
