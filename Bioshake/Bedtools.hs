{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Bedtools(convert) where

import           Bioshake
import           Bioshake.Internal.Bedtools
import           Development.Shake
import           Development.Shake.FilePath

instance IsBam a => Buildable a (Convert "bam" "bed") where
  build params (paths -> [input]) [out] =
    cmd "bedtools bamtobed" ["-i", input] (FileStdout out)

instance (IsPairedEnd a, IsBam a) => Buildable a (Convert "bam" "bedpe") where
  build params (paths -> [input]) [out] =
    cmd "bedtools bamtobed" ["-i", input] "-bedpe" (FileStdout out)

instance (Referenced a, IsBed a) => Buildable a (Convert "bed" "bam") where build = bedtobam
instance (Referenced a, IsVCF a) => Buildable a (Convert "vcf" "bam") where build = bedtobam
instance (Referenced a, IsGff a) => Buildable a (Convert "gff" "bam") where build = bedtobam

bedtobam params a@(paths -> [input]) [out] = do
  let genome = getRef a -<.> "genome"
  need [genome]
  cmd "bedtools bedtobam" ["-i", input] ["-g", genome] (FileStdout out)

instance IsBed a => Buildable a (Intersect "bed") where
  build params (paths -> [input1, input2]) [out] =
    cmd "bedtools intersect" ["-a", input1] ["-b", input2] (FileStdout out)

instance IsBam a => Buildable a (Intersect "bam") where
  build params (paths -> [input1, input2]) [out] =
    cmd "bedtools intersect" ["-a", input1] ["-b", input2] (FileStdout out)
