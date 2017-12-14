{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Octopus(call, normalSample, fast, veryFast, debug, noFilter) where

import           Bioshake
import           Bioshake.Internal.Octopus
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

call :: (Implicit Threads, Implicit [OctopusOpts]) => Call Threads
call = Call param param

$(makeThreaded' ''Call [''Referenced, ''IsBam, ''Sorted] 'buildOctopus)
