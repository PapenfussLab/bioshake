{-# LANGUAGE TemplateHaskell, FlexibleInstances, ViewPatterns #-}
module Bioshake.Internal.Platypus where

import Bioshake
import Bioshake.TH
import Development.Shake
import Development.Shake.FilePath
import Data.List

data Call c = Call c

buildPlatypus _ a@(paths -> inputs) [out] =
  run "platypus callVariants"
    ["--bamFiles=", intercalate "," inputs]
    ["--refFile=", getRef a]
    ["--output=", out]

$(makeSingleTypes ''Call [''IsVCF] [])
