{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.FREEC where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.FREEC
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeCluster ''CNVExome [''Referenced, ''Capture, ''IsMPileup] 'buildFREECExome)
