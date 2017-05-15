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
{- $call Call CNVs for whole exomes using ADTEx. Expects deduplicated BAM files as input, and a capture region. -}
