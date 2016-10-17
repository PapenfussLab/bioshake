{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, KindSignatures, GADTs #-}
module Bioshake.Samtools(sort, sortBam, sortSam, mappedOnly, convert, sam2bam, bam2sam, dedup) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import GHC.TypeLits

data Sort :: Symbol -> * where
  Sort :: Sort a
data DeDup = DeDup
data MappedOnly = MappedOnly
data Pileup = Pileup

data Convert :: Symbol -> Symbol -> * where
  Convert :: Convert a b

instance Pathable a => Pathable (a :-> Sort t) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> "sorted.bam"]
instance Pathable a => Pathable (a :-> MappedOnly) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".mapped_only.bam"]
instance Pathable a => Pathable (a :-> DeDup) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".dedup.bam"]
instance Pathable a => Pathable (a :-> Pileup) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".pileup.bcf"]

instance Pathable a => Pathable (a :-> Convert s "sam") where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> "sam"]
instance Pathable a => Pathable (a :-> Convert s "bam") where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> "bam"]

sort = Sort
sortBam = Sort :: Sort "bam"
sortSam = Sort :: Sort "sam"
mappedOnly = MappedOnly
dedup = DeDup
convert = Convert
sam2bam = Convert :: Convert "sam" "bam"
bam2sam = Convert :: Convert "bam" "sam"

instance Pathable a => IsSorted (a :-> Sort t)
instance Pathable a => IsBam (a :-> Sort t)
instance Pathable a => IsBam (a :-> MappedOnly)
instance Pathable a => IsBam (a :-> Convert s "bam")
instance Pathable a => IsSam (a :-> Convert s "sam")

instance Pathable a => IsBam (a :-> DeDup)
instance Pathable a => IsSorted (a :-> DeDup)

instance Pathable a => IsBcf (a :-> Pileup)

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
