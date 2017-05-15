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

-- | Provides shake rules to build indices for bam files. This is needed for some stages that deal with bam files and require an index.
indexRules =
  "//*.bam.bai" %> \out -> do
    let input = dropExtension out
    lift $ need [input]
    run "samtools index" [input] [out]

$(makeSingleThread ''AddRGLine [''IsBam] 'buildAddRGLine)
$(makeThreaded ''SortBam [''IsBam] 'buildSortBam)
$(makeThreaded ''MappedOnly [''IsSam] 'buildMappedOnly)
$(makeSingleThread ''Pileup [''IsBam, ''Referenced] 'buildPileup)
$(makeSingleThread ''DeDup [''IsBam] 'buildDedup)
$(makeSingleThread ''BedCov [''IsBam, ''Capture] 'buildBedCov)
{- $bedCov Computes coverage for each capture region -}
