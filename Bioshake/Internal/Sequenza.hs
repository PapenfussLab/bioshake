{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Sequenza where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

data Pileup2Seqz c = Pileup2Seqz c deriving Show

class GC a where
  getGC :: a -> FilePath

instance GC a => GC (a :-> b) where
  getGC (a :-> _) = getGC a

instance GC a => GC (All a) where
  getGC (All as) = foldl1 (\l r -> if l == r then l else error "cannot combine mixed reference gc files") $ fmap getGC as

buildPileup2Seqz _ a@(paths -> [normal, tumour]) [out] = do
  let gc = getGC a
  lift $ need [gc]
  run "sequenza-utils bam2seqz -p"
    ["-n", normal]
    ["-t", tumour]
    ["-gc", gc]
    ["-o", out]

instance Pathable a => Pathable (a :-> Pileup2Seqz c) where
  paths (a :-> _) = [hashPath (paths a) <.> "Sequenza" <.> "Pileup2Seqz" <.> "seqz" <.> "gz"]
instance IsSeqzGZ (a :-> Pileup2Seqz c)

data Bin c = Bin c Int

buildBin (Bin _ bs) (paths -> [input]) [out] =
  run "sequenza-utils seqz_binning"
    ["-w", show bs]
    ["-s", input]
    "|gzip > "
    [out]

$(makeSingleTypes ''Bin [''IsSeqzGZ] [])
