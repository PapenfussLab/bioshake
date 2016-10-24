{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts, TemplateHaskell #-}
module Bioshake.BWA where

import Bioshake
import Bioshake.Internal.BWA
import Development.Shake
import Bioshake.Implicit
import Bioshake.TH

$(makeThreaded ''Align [''Referenced, ''IsFastQ] 'buildBWA)
