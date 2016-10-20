{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, GADTs, FlexibleContexts #-}
module Bioshake.Cluster.Samtools(sort, sortBam, sortSam, mappedOnly, convert, sam2bam, bam2sam, dedup) where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Internal.Samtools
import Bioshake.Cluster.Torque
import Bioshake.Implicit

sort :: Implicit_ Config => Sort Config t
sort = Sort param_

sortBam :: Implicit_ Config => Sort Config "bam"
sortBam = sort

sortSam :: Implicit_ Config => Sort Config "sam"
sortSam = sort

mappedOnly :: Implicit_ Config => MappedOnly Config
mappedOnly = MappedOnly param_

convert :: Implicit_ Config => Convert Config s t
convert = Convert param_

sam2bam :: Implicit_ Config => Convert Config "sam" "bam"
sam2bam = convert

bam2sam :: Implicit_ Config => Convert Config "bam" "sam"
bam2sam = convert

dedup :: Implicit_ Config => DeDup Config
dedup = DeDup param_

instance IsSam a => Buildable a (Sort Config "sam") where
  build (Sort config) a@(paths -> [input]) [out] =
    submit "samtools view -b" [input] "|" "samtools sort -" ["-@", show (getCPUs config)] ["-o", out]
      config

instance IsBam a => Buildable a (Sort Config "bam") where
  build (Sort config) a@(paths -> [input]) [out] =
    submit "samtools sort" [input] ["-@", show (getCPUs config)] ["-o", out]
      config

instance IsSam a => Buildable a (Convert Config "sam" "bam") where
  build (Convert config) a@(paths -> [input]) [out] =
    submit "samtools view -b" [input] ["-@", show (getCPUs config)] ["-o", out]
      config

instance IsBam a => Buildable a (Convert Config "bam" "sam") where
  build (Convert config) a@(paths -> [input]) [out] =
    submit "samtools view -h" [input] ["-@", show (getCPUs config)] ["-o", out]
      config

instance IsSam a => Buildable a (MappedOnly Config) where
  build (MappedOnly config) a@(paths -> [input]) [out] =
    submit "samtools view -F 4 -b" [input] ["-@", show (getCPUs config)] ["-o", out]
      config

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a (DeDup Config) where
  build (DeDup config) (paths -> [input]) [out] =
    submit "samtools rmdup" [input] [out]
      config
      (CPUs 1)

instance (Referenced a, IsSam a) => Buildable a (Pileup Config) where
  build (Pileup config) a@(paths -> [input]) [out] =
    submit "samtools mpileup -ug" ["-f", getRef a] [input] ["-o", out]
      config
      (CPUs 1)
