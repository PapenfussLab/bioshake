{-# LANGUAGE TypeOperators, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}
module Bioshake.Internal.GATK where

import Bioshake
import Development.Shake.FilePath

data ReQual = ReQual FilePath

reQual = ReQual

instance Pathable a => Pathable (a :-> ReQual) where
  paths ((paths -> [p]) :-> _) = [p <.> "gatk.requal.bam"]

instance Pathable a => IsBam (a :-> ReQual)
instance (Pathable a, IsSorted a) => IsSorted (a :-> ReQual)
