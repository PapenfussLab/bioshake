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

data Sort :: * -> Symbol -> * where
  Sort :: c -> Sort c a
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

$(makeSingleTypes ''AddRGLine [''IsBam, ''HasRG] [''IsSorted])

data Convert :: * -> Symbol -> Symbol -> * where
  Convert :: c -> Convert c a b

instance Pathable a => Pathable (a :-> Sort c t) where
  paths ((paths -> [a]) :-> _) = [hashPath a <.> "sorted.bam"]
instance Pathable a => Pathable (a :-> MappedOnly c ) where
  paths ((paths -> [a]) :-> _) = [hashPath a <.> ".mapped_only.bam"]
instance Pathable a => Pathable (a :-> DeDup c) where
  paths ((paths -> [a]) :-> _) = [hashPath a <.> ".dedup.bam"]
instance Pathable a => Pathable (a :-> Pileup c) where
  paths ((paths -> a) :-> _) = [hashPath (concat a) <.> ".pileup.bcf"]

instance (KnownSymbol t, Pathable a) => Pathable (a :-> Convert c s t) where
  paths ((paths -> [a]) :-> _) = ["tmp" </> takeFileName a <.> symbolVal (Proxy :: Proxy t)]

instance Pathable a => IsSorted (a :-> Sort c t)
instance Pathable a => IsBam (a :-> Sort c t)
instance Pathable a => IsBam (a :-> MappedOnly c)
instance Pathable a => IsBam (a :-> Convert c s "bam")
instance Pathable a => IsSam (a :-> Convert c s "sam")
instance Pathable a => IsBam (a :-> DeDup c)
instance Pathable a => IsSorted (a :-> DeDup c)
instance Pathable a => IsMPileup (a :-> Pileup c)
