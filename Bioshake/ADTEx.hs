{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.ADTEx where

import           Bioshake
import           Bioshake.Internal.ADTEx
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleThread ''Call [''Capture, ''IsBam, ''DeDuped] 'buildADTEx)
