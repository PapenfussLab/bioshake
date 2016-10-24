{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, GADTs, TemplateHaskell #-}
module Bioshake.Picard where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Internal.Picard
import Bioshake.TH

$(makeSingleThread ''MarkDups [''IsSorted, ''IsPairedEnd, ''IsBam] 'buildMarkDups)
$(makeSingleThread ''DeDup [''IsSorted, ''IsPairedEnd, ''IsBam] 'buildDeDup)
