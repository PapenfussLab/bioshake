{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module introduces a set of common tags that are frequently used when
-- defining stages.
module Bioshake.Tags where

import           Bioshake.Types
import           Control.Monad
import           Language.Haskell.TH

-- Filetypes

-- | Duplicated reads have been removed
class DeDuped a
instance DeDuped a => DeDuped (a :-> b)

-- | Contains an RG line
class HasRG a
instance (HasRG a, IsBam (a :-> b)) => HasRG (a :-> b)

-- | Results from a paired end sequencing processed
class PairedEnd a
instance PairedEnd a => PairedEnd (a :-> b)

-- | Sorted (e.g., sorted sam/bam, or bed)
class Sorted a

-- | Name sorted
class NameSorted a

-- | Filted to a capture region
class CaptureOnly a

class IsBam a
class IsBcf a
class IsBed a
class IsCSV a
class IsFastQ a
class IsGff a
class IsMPileup a
class IsSam a
class IsSeqzGZ a
class IsTSV a
class IsVCF a
class IsTGZ a
class IsCov a


allTags = [''IsFastQ
          ,''DeDuped
          ,''HasRG
          ,''IsBam
          ,''IsBcf
          ,''IsBed
          ,''IsCSV
          ,''IsGff
          ,''IsMPileup
          ,''IsSam
          ,''IsSeqzGZ
          ,''IsTSV
          ,''IsVCF
          ,''IsTGZ
          ,''IsCov
          ,''PairedEnd
          ,''Sorted
          ,''NameSorted]

-- Tagging TH

-- | Template haskell to declare transitivity for all tags through a pipe ending
-- in a type. Examples include 'Bioshake.Out'.
allTransTagsPipe ty =
  forM allTags $ \tag -> do
    a <- newName "a"
    return $ InstanceD Nothing [AppT (ConT tag) (VarT a)] (AppT (ConT tag) (AppT (AppT (ConT ''(:->)) (VarT a)) (ConT ty) )) []

-- | Same as 'allTransTagsPipe' but for transitivity through a constructor.
-- Examples include 'Bioshake.All'.
allTransTags ty =
  forM allTags $ \tag -> do
    a <- newName "a"
    return $ InstanceD Nothing [AppT (ConT tag) (VarT a)] (AppT (ConT tag) (AppT (ConT ty) (VarT a))) []
