{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Samtools where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.Internal.Samtools
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleThread ''AddRGLine [''IsBam] 'buildAddRGLine)
$(makeThreaded ''SortBam [''IsBam] 'buildSortBam)
$(makeThreaded ''MappedOnly [''IsSam] 'buildMappedOnly)
$(makeSingleThread ''Pileup [''IsBam, ''Referenced] 'buildPileup)
$(makeSingleThread ''DeDup [''IsBam] 'buildDedup)
$(makeSingleThread ''BedCov [''IsBam, ''Capture] 'buildBedCov)
{- $bedCov Computes coverage for each capture region -}
