{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, GADTs, ScopedTypeVariables #-}
module Bioshake.Internal.Picard where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Data.Maybe
import System.IO.Temp

data MarkDups = MarkDups FilePath
data DeDup = DeDup FilePath

markdups = MarkDups
dedup = DeDup

instance Pathable a => Pathable (a :-> DeDup) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "picard.DeDup.bam"]

instance Pathable a => Pathable (a :-> MarkDups) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "picard.markdups.bam"]

instance Pathable a => IsBam (a :-> MarkDups)
instance Pathable a => IsBam (a :-> DeDup)

instance (Pathable a, IsSorted a) => IsSorted (a :-> MarkDups)
instance (Pathable a, IsSorted a) => IsSorted (a :-> DeDup)
