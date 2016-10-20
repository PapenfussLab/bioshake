{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.BWA(align, Align(..)) where

import Bioshake
import Bioshake.Internal.BWA
import Development.Shake
import Bioshake.Implicit

align :: Implicit_ Threads => Align Threads
align = Align param_

instance (Referenced a, IsFastQ a) => Buildable a (Align Threads) where
  threads _ (Align (Threads t)) = t
  build (Align (Threads t)) a@(paths -> inputs) [out] =
    cmd "bwa mem"
      ["-t", show t]
      [getRef a]
      inputs
      (FileStdout out)
