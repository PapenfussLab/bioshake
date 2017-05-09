{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cluster.Varscan where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.Varscan
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleCluster ''CallSomatic [''IsMPileup] 'buildVarscan)
$(makeSingleCluster ''CopyNumber [''IsMPileup] 'buildCopyNumber)
