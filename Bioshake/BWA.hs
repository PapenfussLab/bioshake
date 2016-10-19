{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.BWA(align, Align(..)) where

import Bioshake
import Bioshake.Internal.BWA
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Implicit

instance (Referenced a, IsFastQ a) => Buildable a Align where
  threads _ (Align (Threads t)) = t
  build b a@(paths -> inputs) [out] =
    cmd "bwa mem"
      ["-t", show $ threads a b]
      [getRef a]
      inputs
      (FileStdout out)
