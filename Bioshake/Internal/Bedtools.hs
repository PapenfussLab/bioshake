{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, KindSignatures, GADTs, ScopedTypeVariables #-}
module Bioshake.Internal.Bedtools where

import Bioshake
import Development.Shake.FilePath
import GHC.TypeLits
import Data.Proxy

data Convert :: Symbol -> Symbol -> * where
  Convert :: Convert s t

convert = Convert

instance (KnownSymbol t, Pathable a) => Pathable (a :-> Convert s t) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "bamtools" <.> symbolVal (Proxy :: Proxy t)]

instance Pathable a => IsBed (a :-> Convert s "bed")
instance Pathable a => IsBed (a :-> Convert s "bedpe")
instance Pathable a => IsFastQ (a :-> Convert s "fastq")
instance Pathable a => IsBam (a :-> Convert s "bam")

data Intersect :: Symbol -> * where
  Intersect :: Intersect t

intersect = Intersect


instance (KnownSymbol t, Pathable a) => Pathable (a :-> Intersect t) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "bamtools.intersect" <.> symbolVal (Proxy :: Proxy t)]

instance Pathable a => IsBed (a :-> Intersect "bed")
instance Pathable a => IsBam (a :-> Intersect "bam")
