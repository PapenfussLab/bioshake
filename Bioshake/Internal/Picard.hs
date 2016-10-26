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
