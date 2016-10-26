{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cluster.Picard where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Implicit
import           Bioshake.Internal.Picard
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleCluster ''MarkDups [''IsSorted, ''IsPairedEnd, ''IsBam] 'buildMarkDups)
$(makeSingleCluster ''DeDup [''IsSorted, ''IsPairedEnd, ''IsBam] 'buildDeDup)
$(makeSingleCluster ''FixMates [''IsSorted, ''IsPairedEnd, ''IsBam] 'buildFixMates)
