{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, GADTs, ScopedTypeVariables #-}
module Bioshake.Internal.Picard where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Data.Maybe
import System.IO.Temp

data MarkDups c = MarkDups c FilePath
data DeDup c = DeDup c FilePath

instance Pathable a => Pathable (a :-> DeDup c) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "picard.DeDup.bam"]

instance Pathable a => Pathable (a :-> MarkDups c) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "picard.markdups.bam"]

instance Pathable a => IsBam (a :-> MarkDups c)
instance Pathable a => IsBam (a :-> DeDup c)

instance (Pathable a, IsSorted a) => IsSorted (a :-> MarkDups c)
instance (Pathable a, IsSorted a) => IsSorted (a :-> DeDup c)
