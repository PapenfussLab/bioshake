{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cluster.BWA where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Implicit
import           Bioshake.Internal.BWA
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

indexRules :: Implicit Config => Rules ()
indexRules =
  ["//*" <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]] &%> \(o:_) -> do
    let i = dropExtension o
    need [i]
    withSubmit (run "bwa index" [i]) [Left param]

$(makeCluster ''Align [''Referenced, ''IsFastQ] 'buildBWA)
