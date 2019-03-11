{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.Strelka where

import           Bioshake
import           Bioshake.Internal.Strelka
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeCluster ''CallSomatic [''Referenced, ''IsBam, ''Sorted] 'buildStrelkaSomatic)
