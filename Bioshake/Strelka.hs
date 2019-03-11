{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Strelka where

import           Bioshake
import           Bioshake.Internal.Strelka
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeThreaded ''CallSomatic [''Referenced, ''IsBam, ''Sorted] 'buildStrelkaSomatic)
{- $call Call variants using Strelka. Can call multiple samples. -}
