{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Samtools where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.Internal.Samtools
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

-- | Provides shake rules to build indices for bam files. This is needed for some stages that deal with bam files and require an index.
indexRules = do
  "//*.bai" %> \out -> do
    let input = dropExtension out
    need [input]
    cmd "samtools index" [input] [out]
  "//*.fai" %> \out -> do
    let input = dropExtension out
    need [input]
    cmd "samtools faidx" [input]

$(makeSingleThread ''AddRGLine [''IsBam] 'buildAddRGLine)
$(makeThreaded ''SortBam [''IsBam] 'buildSortBam)
$(makeThreaded ''Sam2Bam [''IsSam] 'buildSam2Bam)
$(makeThreaded ''MappedOnly [''IsSam] 'buildMappedOnly)
$(makeSingleThread ''Pileup [''IsBam, ''Referenced, ''Sorted] 'buildPileup)
$(makeSingleThread ''DeDup [''IsBam] 'buildDedup)
$(makeSingleThread ''BedCov [''IsBam, ''Capture] 'buildBedCov)
{- $bedCov Computes coverage for each capture region -}
