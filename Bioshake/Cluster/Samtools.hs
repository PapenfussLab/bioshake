{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cluster.Samtools where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.Samtools
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

indexRules :: Given Config => Rules ()
indexRules = do
  "//*.bai" %> \out -> do
    let input = dropExtension out
    need [input]
    withSubmit (run "samtools index" [input] [out]) [Left given]
  "//*.fai" %> \out -> do
    let input = dropExtension out
    need [input]
    withSubmit (run "samtools faidx" [input]) [Left given]


$(makeSingleCluster ''AddRGLine [''IsBam] 'buildAddRGLine)
$(makeCluster ''SortBam [''IsBam] 'buildSortBam)
$(makeCluster ''Sam2Bam [''IsSam] 'buildSam2Bam)
$(makeCluster ''MappedOnly [''IsSam] 'buildMappedOnly)
$(makeSingleCluster ''Pileup [''IsBam, ''Referenced, ''Sorted] 'buildPileup)
$(makeSingleCluster ''MarkDups [''IsBam] 'buildMarkDups)
$(makeSingleCluster ''BedCov [''IsBam, ''Capture] 'buildBedCov)
