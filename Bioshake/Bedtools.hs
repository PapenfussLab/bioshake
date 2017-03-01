{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Bedtools where

import           Bioshake
import           Bioshake.Internal.Bedtools
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleThread ''CaptureOnly [''Capture, ''IsVCF] 'buildBedtoolsCaptureOnly)
