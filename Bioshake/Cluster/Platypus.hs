{-# LANGUAGE TemplateHaskell, FlexibleInstances, ViewPatterns, MultiParamTypeClasses, FlexibleContexts #-}
module Bioshake.Cluster.Platypus where

import Bioshake
import Bioshake.TH
import Development.Shake
import Development.Shake.FilePath
import Data.List
import Bioshake.Internal.Platypus

$(makeSingleCluster ''Call [''Referenced, ''IsBam] 'buildPlatypus)
