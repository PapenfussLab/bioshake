{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Samtools(sort, sortBam, sortSam, mappedOnly, convert, sam2bam, bam2sam, dedup, pileup, indexRules) where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.Internal.Samtools
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

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
  build (Sort (Threads t)) a@(paths -> [input]) [out] =
    cmd Shell "samtools view -b" [input] "|" "samtools sort -" ["-@", show t] ["-o", out]

instance IsBam a => Buildable a (Sort Threads "bam") where
  build (Sort (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools sort" [input] ["-@", show t] ["-o", out]

instance IsSam a => Buildable a (Convert Threads "sam" "bam") where
  build (Convert (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools view -b" [input] ["-@", show t] ["-o", out]

instance IsBam a => Buildable a (Convert Threads "bam" "sam") where
  build (Convert (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools view -h" [input] ["-@", show t] ["-o", out]

instance IsSam a => Buildable a (MappedOnly Threads) where
  build (MappedOnly (Threads t)) a@(paths -> [input]) [out] =
    cmd "samtools view -F 4 -b" [input] ["-@", show t] ["-o", out]

instance (IsSorted a, IsPairedEnd a, IsBam a) => Buildable a (DeDup ()) where
  build _ (paths -> [input]) [out] =
    cmd "samtools rmdup" [input] [out]

instance (Referenced a, IsSam a) => Buildable a (Pileup ()) where
  build _ a@(paths -> [input]) [out] =
    cmd "samtools mpileup -ug" ["-f", getRef a] [input] ["-o", out]

indexRules = do
  "//*.bam.bai" %> \out -> do
    let input = dropExtension out
    need [input]
    cmd "samtools index" [input] [out]
