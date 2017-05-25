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

-- | Tags a type with GC content; this is not part of 'Bioshake.Referenced' as it is a sequenza specific format.
class GC a where
  getGC :: a -> FilePath

instance GC a => GC (a :-> b) where
  getGC (a :-> _) = getGC a

instance GC a => GC (All a) where
  getGC (All as) = foldl1 (\l r -> if l == r then l else error "cannot combine mixed reference gc files") $ fmap getGC as

buildPileup2Seqz _ a@(paths -> [normal, tumour]) [out] = do
  let gc = getGC a
  lift $ need [gc]
  run "sequenza-utils pileup2abfreq"
    ["-r", normal]
    ["-s", tumour]
    ["-gc", gc]
    "| gzip >"
    [out]

$(makeSingleTypes ''Pileup2Seqz [''IsSeqzGZ] [])
