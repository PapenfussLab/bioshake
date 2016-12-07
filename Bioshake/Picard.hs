{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Picard where

import           Bioshake
import           Bioshake.Internal.Picard
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleThread ''MarkDups [''Sorted, ''PairedEnd, ''IsBam] 'buildMarkDups)
$(makeSingleThread ''DeDup [''Sorted, ''PairedEnd, ''IsBam] 'buildDeDup)
$(makeSingleThread ''FixMates [''Sorted, ''PairedEnd, ''IsBam] 'buildFixMates)
