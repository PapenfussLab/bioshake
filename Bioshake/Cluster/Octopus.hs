{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.Octopus(call, normalSample, fast, veryFast, debug, noFilter) where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.Octopus
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

call :: (Implicit Config, Implicit [OctopusOpts]) => Call Config
call = Call param param

$(makeCluster' ''Call [''Referenced, ''IsBam, ''Sorted] 'buildOctopus)
