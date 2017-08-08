{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.BWA(indexRules, align, k, bw, d, r, y, c, dc, w, m) where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.Internal.BWA
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

indexRules =
  ["//*" <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]] &%> \(o:_) -> do
    let i = dropExtension o
    need [i]
    cmd "bwa index" [i]

align :: (Implicit Threads, Implicit [BWAOpts]) => Align Threads
align = Align param param

$(makeThreaded' ''Align [''Referenced, ''IsFastQ] 'buildBWA)
{- $align Aligns fastq files against the reference using BWA-mem. -}
