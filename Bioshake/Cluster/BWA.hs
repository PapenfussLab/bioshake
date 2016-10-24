{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts, TemplateHaskell #-}
module Bioshake.Cluster.BWA where

import Bioshake
import Bioshake.Internal.BWA
import Development.Shake
import Bioshake.Implicit
import Bioshake.Cluster.Torque
import Bioshake.TH

$(makeCluster ''Align [''Referenced, ''IsFastQ] 'buildBWA)
