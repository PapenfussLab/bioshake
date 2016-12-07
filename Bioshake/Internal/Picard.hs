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

data MarkDups c = MarkDups c
data DeDup c = DeDup c
data FixMates c = FixMates c

buildMarkDups _ (paths -> [input]) [out] =
  run "picard MarkDuplicates"
    ["I=", input]
    ["O=", out]
    ["M=", out -<.> "txt"]

buildDeDup _ (paths -> [input]) [out] =
  run "picard MarkDuplicates"
    ["I=", input]
    ["O=", out]
    ["M=", out -<.> "txt"]
    "REMOVE_DUPLICATES=true"

buildFixMates _ (paths -> [input]) [out] =
  run "picard FixMateInformation"
    ["I=", input]
    ["O=", out]

$(makeSingleTypes ''MarkDups [''IsBam] [''Sorted])
$(makeSingleTypes ''DeDup [''IsBam, ''DeDuped] [''Sorted])
$(makeSingleTypes ''FixMates [''IsBam] [''Sorted])
