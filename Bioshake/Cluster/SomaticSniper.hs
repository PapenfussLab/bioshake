{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cluster.SomaticSniper where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.SomaticSniper
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleCluster ''CallSomatic [''IsBam,''Referenced] 'buildSomaticSniper)
