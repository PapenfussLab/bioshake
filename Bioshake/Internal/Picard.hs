{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, GADTs, ScopedTypeVariables, TemplateHaskell, ViewPatterns #-}
module Bioshake.Internal.Picard where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Data.Maybe
import System.IO.Temp
import Bioshake.TH

data MarkDups c = MarkDups c FilePath
data DeDup c = DeDup c FilePath

buildMarkDups (MarkDups _ jar) (paths -> [input]) [out] =
  run "java" ["-jar", jar] "MarkDuplicates"
    ["I=", input]
    ["O=", out]
    ["M=", out -<.> "txt"]

buildDeDup (DeDup _ jar) (paths -> [input]) [out] =
  run "java" ["-jar", jar] "MarkDuplicates"
    ["I=", input]
    ["O=", out]
    ["M=", out -<.> "txt"]
    "REMOVE_DUPLICATES=true"

$(makeSingleTypes ''MarkDups [''IsBam] [''IsSorted])
$(makeSingleTypes ''DeDup [''IsBam] [''IsSorted])
