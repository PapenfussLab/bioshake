{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
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

class NoContigs a

data Pileup c where
  Pileup :: (Pathable v, Show v, IsVCF v, Sorted v, NoContigs v) => c -> v -> Pileup c

deriving instance Show c => Show (Pileup c)

buildFacets (Pileup _ vcf) a@(paths -> inputs) [out] = do
  let vcfp = paths vcf
  when (length vcfp /= 1) $
    error "facets requires exactly one vcf file"
  lift $ need vcfp
  run "snp-pileup"
    vcfp
    [out]
    inputs

class IsFacets a

$(makeSingleTypes ''Pileup [''IsFacets] [])


-- Sort VCF file output into lex order
data Sort c = Sort c deriving Show

buildSort :: Pathable a => t -> a -> [FilePath] -> FreeT CmdF Action ()
buildSort _ a@(paths -> [input]) [out] = do
  () <- run "grep '^#'" [input] ">" [out]
  run "grep -v '^#'" [input] "| LC_ALL=C sort -t '\\t' -k1,1 -k2,2n >>" [out]

$(makeSingleTypes ''Sort [''IsVCF, ''Sorted] [''NoContigs])

-- Filters out contigs
data FilterContigs c = FilterContigs c deriving Show

buildNoContigs :: Pathable a => t -> a -> [FilePath] -> FreeT CmdF Action ()
buildNoContigs _ a@(paths -> [input]) [out] = do
  () <- run "grep '^#'" [input] ">" [out]
  run "grep -v '^#'" [input] "| grep '^[^_]*\\t' >>" [out]

$(makeSingleTypes ''FilterContigs [''IsVCF, ''NoContigs] [''Sorted])
