{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, KindSignatures, GADTs, ScopedTypeVariables, FlexibleContexts #-}
module Bioshake.Internal.Samtools where

import Bioshake
import Development.Shake.FilePath
import GHC.TypeLits
import Data.Proxy
import Bioshake.Implicit

data Sort :: Symbol -> * where
  Sort :: Threads -> Sort a
data DeDup = DeDup
data MappedOnly = MappedOnly Threads
data Pileup = Pileup

data Convert :: Symbol -> Symbol -> * where
  Convert :: Threads -> Convert a b

instance Pathable a => Pathable (a :-> Sort t) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> "sorted.bam"]
instance Pathable a => Pathable (a :-> MappedOnly) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".mapped_only.bam"]
instance Pathable a => Pathable (a :-> DeDup) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".dedup.bam"]
instance Pathable a => Pathable (a :-> Pileup) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".pileup.bcf"]

instance (KnownSymbol t, Pathable a) => Pathable (a :-> Convert s t) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> symbolVal (Proxy :: Proxy t)]

sort :: Implicit_ Threads => Sort s
sort = Sort param_

sortBam :: Implicit_ Threads => Sort "bam"
sortBam = sort

sortSam :: Implicit_ Threads => Sort "sam"
sortSam = sort

mappedOnly :: Implicit_ Threads => MappedOnly
mappedOnly = MappedOnly param_

dedup = DeDup

convert :: Implicit_ Threads => Convert s t
convert = Convert param_

sam2bam :: Implicit_ Threads => Convert "sam" "bam"
sam2bam = convert

bam2sam :: Implicit_ Threads => Convert "bam" "sam"
bam2sam = convert

instance Pathable a => IsSorted (a :-> Sort t)
instance Pathable a => IsBam (a :-> Sort t)
instance Pathable a => IsBam (a :-> MappedOnly)
instance Pathable a => IsBam (a :-> Convert s "bam")
instance Pathable a => IsSam (a :-> Convert s "sam")
instance Pathable a => IsBam (a :-> DeDup)
instance Pathable a => IsSorted (a :-> DeDup)
instance Pathable a => IsBcf (a :-> Pileup)
