{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, GADTs, FlexibleContexts #-}
module Bioshake.Cluster.Samtools(sort, sortBam, sortSam, mappedOnly, convert, sam2bam, bam2sam, dedup) where

import Bioshake
import Bioshake.Cluster.Torque
import Development.Shake
import Development.Shake.FilePath
import GHC.TypeLits
import Data.Implicit
import Bioshake.Internal.Samtools

instance (Implicit_ Config, IsSam a) => Buildable a (Sort "sam") where
  build _ (paths -> [input]) [out] =
    submit "samtools view -bS" [input] "|" "samtools sort -" ["-o", out] (param_ :: Config)

instance (Implicit_ Config, IsBam a) => Buildable a (Sort "bam") where
  build _ (paths -> [input]) [out] =
    submit "samtools sort" [input] ["-o", out] (param_ :: Config)

instance (Implicit_ Config, IsSam a) => Buildable a (Convert "sam" "bam") where
  build _ (paths -> [input]) [out] =
    submit "samtools view -bS" [input] ["-o", out] (param_ :: Config)

instance (Implicit_ Config, IsBam a) => Buildable a (Convert "bam" "sam") where
  build _ (paths -> [input]) [out] =
    submit "samtools view -h" [input] ["-o", out] (param_ :: Config)

instance (Implicit_ Config, IsSam a) => Buildable a MappedOnly where
  build _ (paths -> [input]) [out] =
    submit "samtools view -h -F 4 -b" [input] ["-o", out] (param_ :: Config)

instance (Implicit_ Config, IsSorted a, IsPairedEnd a, IsBam a) => Buildable a DeDup where
  build _ (paths -> [input]) [out] =
    submit "samtools rmdup" ["-s", input] ["-o", out] (param_ :: Config)

instance (Implicit_ Config, Referenced a, IsSam a) => Buildable a Pileup where
  build _ a@(paths -> [input]) [out] =
    submit "samtools mpileup -ug" ["-f", getRef a] [input] ["-o", out] (param_ :: Config)
