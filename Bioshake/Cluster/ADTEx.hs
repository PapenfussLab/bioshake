{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.ADTEx where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.ADTEx
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleCluster ''Call [''Capture, ''IsBam, ''DeDuped] 'buildADTEx)
