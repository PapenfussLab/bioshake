{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Octopus(call, callWith, normalSample, fast, veryFast, debug, noFilter) where

import           Bioshake
import           Bioshake.Internal.Octopus
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

callWith :: Given Threads => [OctopusOpts] -> Call Threads
callWith = Call given

call :: Given Threads => Call Threads
call = callWith []

$(makeThreaded' ''Call [''Referenced, ''IsBam, ''Sorted] 'buildOctopus)
