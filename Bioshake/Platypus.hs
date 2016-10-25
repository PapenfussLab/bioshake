{-# LANGUAGE TemplateHaskell, FlexibleInstances, ViewPatterns, MultiParamTypeClasses #-}
module Bioshake.Platypus where

import Bioshake
import Bioshake.TH
import Development.Shake
import Development.Shake.FilePath
import Data.List
import Bioshake.Internal.Platypus

$(makeSingleThread ''Call [''Referenced, ''IsBam] 'buildPlatypus)
