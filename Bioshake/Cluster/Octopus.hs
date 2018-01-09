{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.Octopus(call, callWith, normalSample, fast, veryFast, debug, noFilter) where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.Octopus
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

callWith :: Given Config => [OctopusOpts] -> Call Config
callWith = Call given

call :: Given Config => Call Config
call = callWith []

$(makeCluster' ''Call [''Referenced, ''IsBam, ''Sorted] 'buildOctopus)
