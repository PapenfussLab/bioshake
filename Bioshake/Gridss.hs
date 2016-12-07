{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Gridss where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.Internal.Gridss
import           Bioshake.TH
import           Development.Shake
import           System.Directory
import           System.IO.Temp

$(makeThreaded ''Call [''IsBam, ''Sorted, ''Referenced] 'buildCall)
