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

$(makeSingleThread ''FilterCaptureBam [''Capture, ''IsBam] 'buildBedtoolsCaptureBam)
$(makeSingleThread ''FilterCapture [''Capture, ''IsVCF] 'buildBedtoolsCapture)
{- $captureOnly Reduces VCF files to the capture region only using bedtools. -}

