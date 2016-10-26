{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.Picard where

import           Bioshake
import           Bioshake.TH
import           Data.Maybe
import           Development.Shake
import           Development.Shake.FilePath
import           System.IO.Temp

data MarkDups c = MarkDups c FilePath
data DeDup c = DeDup c FilePath
data FixMates c = FixMates c FilePath

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

buildFixMates (FixMates _ jar) (paths -> [input]) [out] =
  run "java" ["-jar", jar] "FixMateInformation"
    ["I=", input]
    ["O=", out]

$(makeSingleTypes ''MarkDups [''IsBam] [''IsSorted])
$(makeSingleTypes ''DeDup [''IsBam] [''IsSorted])
$(makeSingleTypes ''FixMates [''IsBam] [''IsSorted])
