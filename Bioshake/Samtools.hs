{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, GADTs, FlexibleContexts #-}
module Bioshake.Samtools(sort, sortBam, sortSam, mappedOnly, convert, sam2bam, bam2sam, dedup) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import GHC.TypeLits
import Bioshake.Internal.Samtools
import Bioshake.Implicit

instance IsSam a => Buildable a (Sort "sam") where
  threads _ (Sort (Threads t)) = t
  build (Sort (Threads t)) a@(paths -> [input]) [out] =
    cmd Shell "samtools view -b" [input] "|" "samtools sort -" ["-@", show t] ["-o", out]

instance IsBam a => Buildable a (Sort "bam") where
  threads _ (Sort (Threads t)) = t
  build (Sort (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools sort" [input] ["-@", show t] ["-o", out]

instance IsSam a => Buildable a (Convert "sam" "bam") where
  threads _ (Convert (Threads t)) = t
  build (Convert (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools view -b" [input] ["-@", show t] ["-o", out]

instance IsBam a => Buildable a (Convert "bam" "sam") where
  threads _ (Convert (Threads t)) = t
  build (Convert (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools view -h" [input] ["-@", show t] ["-o", out]

instance IsSam a => Buildable a MappedOnly where
  threads _ (MappedOnly (Threads t)) = t
  build (MappedOnly (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools view -F 4 -b" [input] ["-@", show t] ["-o", out]

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a DeDup where
  build _ (paths -> [input]) [out] =
    cmd "samtools rmdup" [input] [out]

instance (Referenced a, IsSam a) => Buildable a Pileup where
  build _ a@(paths -> [input]) [out] =
    cmd "samtools mpileup -ug" ["-f", getRef a] [input] ["-o", out]
