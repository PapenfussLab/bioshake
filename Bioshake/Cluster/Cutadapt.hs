{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cluster.Cutadapt where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.Cutadapt
import           Bioshake.TH
import           Development.Shake

$(makeSingleCluster ''Trim [''IsFastQ] 'buildTrim)
