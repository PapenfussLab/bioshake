{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Internal.Cutadapt where

import           Bioshake
import           Bioshake.TH
import           Development.Shake.FilePath

data Trim c = Trim c Seq

$(makeMultiTypes ''Trim [''IsFastQ] [])
