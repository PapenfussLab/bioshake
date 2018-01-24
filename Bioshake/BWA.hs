{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.BWA(indexRules, align, alignWith, k, bw, d, r, y, c, dc, w, m, rg, softClip) where

import           Bioshake
import           Bioshake.Internal.BWA
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

indexRules =
  ["//*" <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]] &%> \(o:_) -> do
    let i = dropExtension o
    need [i]
    cmd "bwa index" [i]

alignWith :: Given Threads => [BWAOpts] -> Align Threads
alignWith = Align given

align :: Given Threads => Align Threads
align = alignWith []

$(makeThreaded' ''Align [''Referenced, ''IsFastQ] 'buildBWA)
{- $align Aligns fastq files against the reference using BWA-mem. -}
