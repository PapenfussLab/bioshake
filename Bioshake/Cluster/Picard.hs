{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cluster.Picard where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.Picard
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleCluster ''MarkDups [''Sorted, ''PairedEnd, ''IsBam] 'buildMarkDups)
$(makeSingleCluster ''DeDup [''Sorted, ''PairedEnd, ''IsBam] 'buildDeDup)
$(makeSingleCluster ''FixMates [''Sorted, ''PairedEnd, ''IsBam] 'buildFixMates)
