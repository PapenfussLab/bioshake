{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Hisat2 where

import           Bioshake
import           Bioshake.Internal.Hisat2
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeThreaded ''Align [''Referenced, ''IsFastQ] 'buildHisat2)
