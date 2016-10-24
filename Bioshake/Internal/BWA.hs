{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts, TemplateHaskell, ViewPatterns #-}
module Bioshake.Internal.BWA where

import Bioshake
import Data.List
import Development.Shake.FilePath
import Bioshake.Implicit
import Bioshake.TH

data Align c = Align c

buildBWA t _ a@(paths -> inputs) [out] =
    run "bwa mem"
      ["-t", show t]
      [getRef a]
      inputs
      ">" out

$(makeSingleTypes ''Align [''IsSam] [])
