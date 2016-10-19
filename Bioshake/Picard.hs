{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, GADTs #-}
module Bioshake.Picard(markdups, dedup) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Internal.Picard

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a MarkDups where
  build (MarkDups jar) (paths -> [input]) [out] =
    cmd "java" ["-jar", jar] "MarkDuplicates"
      ["I=", input]
      ["O=", out]
      ["M=", out -<.> "txt"]

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a DeDup where
  build (DeDup jar) (paths -> [input]) [out] =
    cmd "java" ["-jar", jar] "MarkDuplicates"
      ["I=", input]
      ["O=", out]
      ["M=", out -<.> "txt"]
      "REMOVE_DUPLICATES=true"
