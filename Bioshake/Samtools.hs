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

indexRules =
  "//*.bam.bai" %> \out -> do
    let input = dropExtension out
    need [input]
    cmd "samtools index" [input] [out]

$(makeSingleThread ''AddRGLine [''IsBam] 'buildAddRGLine)
$(makeThreaded ''SortBam [''IsBam] 'buildSortBam)
$(makeThreaded ''MappedOnly [''IsSam] 'buildMappedOnly)
$(makeSingleThread ''Pileup [''IsBam, ''Referenced] 'buildPileup)
$(makeSingleThread ''DeDup [''IsBam] 'buildDedup)
