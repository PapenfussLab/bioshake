{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
module Bioshake.Internal.Facets where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Free
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files         (createLink, rename)

class HasBams a where
  bams :: a -> [FilePath]

instance {-# OVERLAPPABLE #-} (IsBam a, Pathable a) => HasBams a where
  bams = paths

instance {-# OVERLAPPING #-} HasBams a => HasBams (a :-> b) where
  bams (a :-> _) = bams a

class NoContigs a

data Pileup c = Pileup c deriving Show

buildFacets :: (Pathable a, HasBams a) => Pileup c -> a -> [FilePath] -> Cmd ()
buildFacets _ a@(paths -> [vcf]) [out] = do
  let inputs = bams a
  lift $ need inputs
  run "snp-pileup"
    vcf
    [out]
    inputs

class IsFacets a

$(makeSingleTypes ''Pileup [''IsFacets] [])


-- Sort VCF file output into lex order
data Sort c = Sort c deriving Show

buildSort :: Pathable a => t -> a -> [FilePath] -> FreeT CmdF Action ()
buildSort _ a@(paths -> [input]) [out] = do
  () <- run "grep '^#'" [input] ">" [out]
  run "grep -v '^#'" [input] "| LC_ALL=C sort -t $'\\t' -k1,1 -k2,2n >>" [out]

$(makeSingleTypes ''Sort [''IsVCF, ''Sorted] [''NoContigs])

-- Filters out contigs
data FilterContigs c = FilterContigs c deriving Show

buildNoContigs :: Pathable a => t -> a -> [FilePath] -> FreeT CmdF Action ()
buildNoContigs _ a@(paths -> [input]) [out] = do
  () <- run "grep '^#'" [input] ">" [out]
  run "grep -v '^#'" [input] "| grep '^[^_]*\\t' >>" [out]

$(makeSingleTypes ''FilterContigs [''IsVCF, ''NoContigs] [''Sorted])
