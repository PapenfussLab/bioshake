{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.BWA where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.Internal.BWA
import           Bioshake.TH
import           Development.Shake

$(makeThreaded ''Align [''Referenced, ''IsFastQ] 'buildBWA)
