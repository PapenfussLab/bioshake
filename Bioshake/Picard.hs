{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, GADTs #-}
module Bioshake.Picard(markdups, dedup) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Internal.Picard

markdups :: FilePath -> MarkDups ()
markdups = MarkDups ()

dedup :: FilePath -> DeDup ()
dedup = DeDup ()

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a (MarkDups ()) where
  build (MarkDups _ jar) (paths -> [input]) [out] =
    cmd "java" ["-jar", jar] "MarkDuplicates"
      ["I=", input]
      ["O=", out]
      ["M=", out -<.> "txt"]

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a (DeDup ()) where
  build (DeDup _ jar) (paths -> [input]) [out] =
    cmd "java" ["-jar", jar] "MarkDuplicates"
      ["I=", input]
      ["O=", out]
      ["M=", out -<.> "txt"]
      "REMOVE_DUPLICATES=true"
