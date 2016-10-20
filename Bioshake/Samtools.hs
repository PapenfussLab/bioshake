{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, GADTs, FlexibleContexts #-}
module Bioshake.Samtools(sort, sortBam, sortSam, mappedOnly, convert, sam2bam, bam2sam, dedup, pileup) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Internal.Samtools
import Bioshake.Implicit

sort :: Implicit_ Threads => Sort Threads t
sort = Sort param_

sortBam :: Implicit_ Threads => Sort Threads "bam"
sortBam = sort

sortSam :: Implicit_ Threads => Sort Threads "sam"
sortSam = sort

mappedOnly :: Implicit_ Threads => MappedOnly Threads
mappedOnly = MappedOnly param_

convert :: Implicit_ Threads => Convert Threads s t
convert = Convert param_

sam2bam :: Implicit_ Threads => Convert Threads "sam" "bam"
sam2bam = convert

bam2sam :: Implicit_ Threads => Convert Threads "bam" "sam"
bam2sam = convert

dedup :: DeDup ()
dedup = DeDup ()

pileup :: Pileup ()
pileup = Pileup ()

instance IsSam a => Buildable a (Sort Threads "sam") where
  threads _ (Sort (Threads t)) = t
  build (Sort (Threads t)) a@(paths -> [input]) [out] =
    cmd Shell "samtools view -b" [input] "|" "samtools sort -" ["-@", show t] ["-o", out]

instance IsBam a => Buildable a (Sort Threads "bam") where
  threads _ (Sort (Threads t)) = t
  build (Sort (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools sort" [input] ["-@", show t] ["-o", out]

instance IsSam a => Buildable a (Convert Threads "sam" "bam") where
  threads _ (Convert (Threads t)) = t
  build (Convert (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools view -b" [input] ["-@", show t] ["-o", out]

instance IsBam a => Buildable a (Convert Threads "bam" "sam") where
  threads _ (Convert (Threads t)) = t
  build (Convert (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools view -h" [input] ["-@", show t] ["-o", out]

instance IsSam a => Buildable a (MappedOnly Threads) where
  threads _ (MappedOnly (Threads t)) = t
  build (MappedOnly (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools view -F 4 -b" [input] ["-@", show t] ["-o", out]

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a (DeDup ()) where
  build _ (paths -> [input]) [out] =
    cmd "samtools rmdup" [input] [out]

instance (Referenced a, IsSam a) => Buildable a (Pileup ()) where
  build _ a@(paths -> [input]) [out] =
    cmd "samtools mpileup -ug" ["-f", getRef a] [input] ["-o", out]
