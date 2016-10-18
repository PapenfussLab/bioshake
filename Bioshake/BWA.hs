{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.BWA(align, Align(..)) where

import Bioshake
import Bioshake.Internal.BWA
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath

instance (Referenced a, IsFastQ a) => Buildable a Align where
  build params a@(paths -> inputs) [out] =
    let cmd' =
          cmd "bwa mem"
            ["-t", show (threads params)]
            [getRef a]
            inputs
            (FileStdout out)
    in case resource params of
         Just res -> withResource res (threads params) cmd'
         Nothing -> cmd'
