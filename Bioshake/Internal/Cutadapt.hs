{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, TemplateHaskell #-}
module Bioshake.Internal.Cutadapt where

import Bioshake
import Development.Shake.FilePath
import Bioshake.TH

data Trim c = Trim c Seq

$(makeMultiTypes ''Trim [''IsFastQ] [])
