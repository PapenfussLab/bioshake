{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.ContextMap where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.Internal.ContextMap
import           Bioshake.TH
import           Development.Shake

$(makeThreaded ''Align [''Referenced, ''IsFastQ] 'buildContextMap)
