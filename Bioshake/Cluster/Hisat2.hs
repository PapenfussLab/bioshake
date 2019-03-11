{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.Hisat2 where

import           Bioshake
import           Bioshake.Internal.Hisat2
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeCluster ''Align [''Referenced, ''IsFastQ] 'buildHisat2)
