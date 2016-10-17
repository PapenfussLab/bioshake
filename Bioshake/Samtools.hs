{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Samtools(sortSam, sortBam, sam2bam, dedup) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath

data SortSam = SortSam
data SortBam = SortBam
data Sam2Bam = Sam2Bam
data Bam2Sam = Bam2Sam
data DeDup = DeDup
data MappedOnly = MappedOnly

instance Pathable a => Pathable (a :-> SortSam) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> "sorted.bam"]
instance Pathable a => Pathable (a :-> SortBam) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> "sorted.bam"]
instance Pathable a => Pathable (a :-> Sam2Bam) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> "bam"]
instance Pathable a => Pathable (a :-> Bam2Sam) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> "sam"]
instance Pathable a => Pathable (a :-> MappedOnly) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".mapped_only.bam"]
instance Pathable a => Pathable (a :-> DeDup) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".dedup.bam"]

sortSam = SortSam
sortBam = SortBam
sam2bam = Sam2Bam
bam2sam = Bam2Sam
mappedOnly = MappedOnly
dedup = DeDup

instance Pathable a => IsSorted (a :-> SortSam)
instance Pathable a => IsSorted (a :-> SortBam)
instance Pathable a => IsBam (a :-> SortSam)
instance Pathable a => IsBam (a :-> SortBam)
instance Pathable a => IsBam (a :-> Sam2Bam)
instance Pathable a => IsSam (a :-> Bam2Sam)
instance Pathable a => IsBam (a :-> MappedOnly)

instance Pathable a => IsBam (a :-> DeDup)
instance Pathable a => IsSorted (a :-> DeDup)

instance IsSam a => Buildable a SortSam where
  build _ (paths -> [input]) [out] =
    cmd Shell "samtools view -bS" [input] "|" "samtools sort -" ["-o", out]

instance IsBam a => Buildable a SortBam where
  build _ (paths -> [input]) [out] =
    cmd "samtools sort" [input] ["-o", out]

instance IsSam a => Buildable a Sam2Bam where
  build _ (paths -> [input]) [out] =
    cmd "samtools view -bS" [input] ["-o", out]

instance IsSam a => Buildable a Bam2Sam where
  build _ (paths -> [input]) [out] =
    cmd "samtools view -h" [input] ["-o", out]

instance IsSam a => Buildable a MappedOnly where
  build _ (paths -> [input]) [out] =
    cmd "samtools view -h -F 4 -b" [input] ["-o", out]

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a DeDup where
  build _ (paths -> [input]) [out] =
    cmd "samtools rmdup" ["-s", input] ["-o", out]
