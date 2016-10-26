{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Platypus where

import           Bioshake
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

data Call c = Call c

buildPlatypus _ a@(paths -> inputs) [out] =
  run "platypus callVariants"
    ["--bamFiles=", intercalate "," inputs]
    ["--refFile=", getRef a]
    ["--output=", out]

$(makeSingleTypes ''Call [''IsVCF] [])
