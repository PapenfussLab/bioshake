{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cluster.BWA(indexRules, align, alignWith, k, bw, d, r, y, c, dc, w, m, rg) where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.BWA
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

indexRules :: Given Config => Rules ()
indexRules =
  ["//*" <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]] &%> \(o:_) -> do
    let i = dropExtension o
    need [i]
    withSubmit (run "bwa index" [i]) [Left given]

alignWith :: Given Config => [BWAOpts] -> Align Config
alignWith = Align given

align :: Given Config => Align Config
align = alignWith []

$(makeCluster' ''Align [''Referenced, ''IsFastQ] 'buildBWA)
