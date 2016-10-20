{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, KindSignatures, GADTs, ScopedTypeVariables, FlexibleContexts #-}
module Bioshake.Internal.Samtools where

import Bioshake
import Development.Shake.FilePath
import GHC.TypeLits
import Data.Proxy
import Bioshake.Implicit

data Sort :: * -> Symbol -> * where
  Sort :: c -> Sort c a
data DeDup c = DeDup c
data MappedOnly c = MappedOnly c
data Pileup c = Pileup c

data Convert :: * -> Symbol -> Symbol -> * where
  Convert :: c -> Convert c a b

instance Pathable a => Pathable (a :-> Sort c t) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> "sorted.bam"]
instance Pathable a => Pathable (a :-> MappedOnly c ) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".mapped_only.bam"]
instance Pathable a => Pathable (a :-> DeDup c) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".dedup.bam"]
instance Pathable a => Pathable (a :-> Pileup c) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> ".pileup.bcf"]

instance (KnownSymbol t, Pathable a) => Pathable (a :-> Convert c s t) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> symbolVal (Proxy :: Proxy t)]


instance Pathable a => IsSorted (a :-> Sort c t)
instance Pathable a => IsBam (a :-> Sort c t)
instance Pathable a => IsBam (a :-> MappedOnly c)
instance Pathable a => IsBam (a :-> Convert c s "bam")
instance Pathable a => IsSam (a :-> Convert c s "sam")
instance Pathable a => IsBam (a :-> DeDup c)
instance Pathable a => IsSorted (a :-> DeDup c)
instance Pathable a => IsBcf (a :-> Pileup c)
