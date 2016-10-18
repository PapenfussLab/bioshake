{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, GADTs #-}
module Bioshake.Samtools(sort, sortBam, sortSam, mappedOnly, convert, sam2bam, bam2sam, dedup) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import GHC.TypeLits
import Bioshake.Internal.Samtools

instance IsSam a => Buildable a (Sort "sam") where
  build _ (paths -> [input]) [out] =
    cmd Shell "samtools view -bS" [input] "|" "samtools sort -" ["-o", out]

instance IsBam a => Buildable a (Sort "bam") where
  build _ (paths -> [input]) [out] =
    cmd "samtools sort" [input] ["-o", out]

instance IsSam a => Buildable a (Convert "sam" "bam") where
  build _ (paths -> [input]) [out] =
    cmd "samtools view -bS" [input] ["-o", out]

instance IsBam a => Buildable a (Convert "bam" "sam") where
  build _ (paths -> [input]) [out] =
    cmd "samtools view -h" [input] ["-o", out]

instance IsSam a => Buildable a MappedOnly where
  build _ (paths -> [input]) [out] =
    cmd "samtools view -h -F 4 -b" [input] ["-o", out]

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a DeDup where
  build _ (paths -> [input]) [out] =
    cmd "samtools rmdup" ["-s", input] ["-o", out]

instance (Referenced a, IsSam a) => Buildable a Pileup where
  build _ a@(paths -> [input]) [out] =
    cmd "samtools mpileup -ug" ["-f", getRef a] [input] ["-o", out]
