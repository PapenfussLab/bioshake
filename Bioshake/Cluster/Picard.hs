{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, GADTs, FlexibleContexts, TemplateHaskell #-}
module Bioshake.Cluster.Picard where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Internal.Picard
import Bioshake.Implicit
import Bioshake.Cluster.Torque
import Bioshake.TH

$(makeSingleCluster ''MarkDups [''IsSorted, ''IsPairedEnd, ''IsBam] 'buildMarkDups)
$(makeSingleCluster ''DeDup [''IsSorted, ''IsPairedEnd, ''IsBam] 'buildDeDup)
