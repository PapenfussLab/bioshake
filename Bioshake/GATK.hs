{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.GATK where

import           Bioshake
import           Bioshake.Internal.GATK
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath
import           System.IO.Temp

$(makeSingleThread ''ReQual [''IsBam, ''Referenced] 'buildReQual)
$(makeSingleThread ''RealignIndels [''IsBam, ''Referenced] 'buildRealignIndels)
