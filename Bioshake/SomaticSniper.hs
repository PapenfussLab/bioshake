{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.SomaticSniper where

import           Bioshake
import           Bioshake.Internal.SomaticSniper
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleThread ''CallSomatic [''IsBam, ''Referenced] 'buildSomaticSniper)
