{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Platypus where

import           Bioshake
import           Bioshake.Internal.Platypus
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeThreaded ''Call [''Referenced, ''IsBam] 'buildPlatypus)
{- $call Call variants using Platypus. Can call multiple samples. -}
