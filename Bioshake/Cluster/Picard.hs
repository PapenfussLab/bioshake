{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, GADTs, FlexibleContexts #-}
module Bioshake.Cluster.Picard(markdups, dedup) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Internal.Picard
import Bioshake.Implicit
import Bioshake.Cluster.Torque

markdups :: Implicit_ Config => FilePath -> MarkDups Config
markdups = MarkDups param_

dedup :: Implicit_ Config => FilePath -> DeDup Config
dedup = DeDup param_

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a (MarkDups Config) where
  build (MarkDups config jar) (paths -> [input]) [out] =
    submit "java" ["-jar", jar] "MarkDuplicates"
      ["I=", input]
      ["O=", out]
      ["M=", out -<.> "txt"]
      config
      (CPUs 1)

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a (DeDup Config) where
  build (DeDup config jar) (paths -> [input]) [out] =
    submit "java" ["-jar", jar] "MarkDuplicates"
      ["I=", input]
      ["O=", out]
      ["M=", out -<.> "txt"]
      "REMOVE_DUPLICATES=true"
      config
      (CPUs 1)
