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
import           Bioshake.Implicit
import           Bioshake.TH
import           Data.Proxy
import           Development.Shake.FilePath
import           GHC.TypeLits

data SortBam c = SortBam c
data DeDup c = DeDup c
data MappedOnly c = MappedOnly c
data Pileup c = Pileup c
data AddRGLine c = AddRGLine c String

buildAddRGLine (AddRGLine _ name) (paths -> [input]) [out] =
  run "samtools addreplacerg"
    ["-r", concat ["'ID:", name, "\tSM:", name, "'"]]
    input
    ["-o", out]
    "-O bam"

$(makeSingleTypes ''AddRGLine [''IsBam, ''HasRG] [''Sorted])

buildSortBam t _ (paths -> [input]) [out] =
  run "samtools sort" [input] ["-@", show t] ["-o", out]

$(makeSingleTypes ''SortBam [''IsBam, ''Sorted] [])

buildMappedOnly t _ (paths -> [input]) [out] =
  run "samtools view -F 4 -b" [input] ["-@", show t] ["-o", out]

$(makeSingleTypes ''MappedOnly [''IsBam] [])

buildPileup _ a@(paths -> inputs) [out] =
  run "samtools mpileup -q1 -B" ["-f", getRef a] inputs ["-o", out]

$(makeSingleTypes ''Pileup [''IsMPileup] [])

buildDedup _ (paths -> [input]) [out] =
  run "samtools rmdup" [input] [out]

$(makeSingleTypes ''DeDup [''IsBam] [])
