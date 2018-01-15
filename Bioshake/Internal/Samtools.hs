{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.Samtools where

import           Bioshake
import           Bioshake.TH
import           Control.Monad.Trans        (lift)
import           Data.Proxy
import           Development.Shake
import           Development.Shake.FilePath
import           GHC.TypeLits

class MS c

data Sam2Bam c = Sam2Bam c deriving Show
data SortBam c = SortBam c deriving Show
data NameSortBam c = NameSortBam c deriving Show
data FixMates c = FixMates c deriving Show
data MarkDups c = MarkDups c deriving Show
data MappedOnly c = MappedOnly c deriving Show
data Pileup c = Pileup c deriving Show
data AddRGLine c = AddRGLine c String deriving Show
data BedCov c = BedCov c deriving Show

buildAddRGLine (AddRGLine _ name) (paths -> [input]) [out] =
  run "samtools addreplacerg"
    ["-r", concat ["'ID:", name, "\tSM:", name, "'"]]
    input
    ["-o", out]
    "-O bam"

$(makeSingleTypes ''AddRGLine [''IsBam, ''HasRG] [''Sorted, ''MS])

buildSortBam t _ (paths -> [input]) [out] =
  run "samtools sort" [input] ["-@", show t] ["-o", out]

$(makeSingleTypes ''SortBam [''IsBam, ''Sorted] [''MS])

buildNameSortBam t _ (paths -> [input]) [out] =
  run "samtools sort -n" [input] ["-@", show t] ["-o", out]

$(makeSingleTypes ''NameSortBam [''IsBam, ''NameSorted] [''MS])

buildSam2Bam t _ (paths -> [input]) [out] =
  run "samtools view -b" [input] ["-@", show t] ["-o", out]

$(makeSingleTypes ''Sam2Bam [''IsBam] [''MS])

buildMappedOnly t _ (paths -> [input]) [out] =
  run "samtools view -F 4 -b" [input] ["-@", show t] ["-o", out]

$(makeSingleTypes ''MappedOnly [''IsBam] [''Sorted, ''MS])

buildPileup _ a@(paths -> inputs) [out] =
  run "samtools mpileup -q1 -B" ["-f", getRef a] inputs ["-o", out]

$(makeSingleTypes ''Pileup [''IsMPileup] [])

buildFixMates _ (paths -> [input]) [out] =
  run "samtools fixmate -m" [input] [out]

$(makeSingleTypes ''FixMates [''IsBam, ''MS] [''NameSorted])

buildMarkDups _ (paths -> [input]) [out] =
  run "samtools markdup" [input] [out]

$(makeSingleTypes ''MarkDups [''IsBam] [''Sorted, ''MS])

buildBedCov _ a@(paths -> inputs) [out] = do
  let bed = getBED a
      bais = map ( <.> "bai" ) inputs
  lift . need $ bed : bais
  run "samtools bedcov" [bed] inputs ">" [out]

$(makeSingleTypes ''BedCov [''IsCov] [])
