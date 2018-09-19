{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.Bedtools where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.Bedtools
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleCluster ''FilterCaptureBam [''Capture, ''IsVCF] 'buildBedtoolsCaptureBam)
$(makeSingleCluster ''FilterCapture [''Capture, ''IsBam] 'buildBedtoolsCapture)
