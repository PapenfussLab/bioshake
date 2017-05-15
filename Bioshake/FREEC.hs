{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.FREEC where

import           Bioshake
import           Bioshake.Internal.FREEC
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeThreaded ''CNVExome [''Referenced, ''Capture, ''IsMPileup] 'buildFREECExome)
{- $CNVExome CNV calling on exomes using FREEC -}
