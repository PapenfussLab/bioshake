{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Bioshake.Tags where

import           Bioshake.Types
import           Control.Monad
import           Language.Haskell.TH

-- Filetypes

class DeDuped a
class HasRG a
class IsBam a
class IsBcf a
class IsBed a
class IsCSV a
class IsFastQ a
class IsGff a
class IsMPileup a
class IsSam a
class IsTSV a
class IsVCF a
class PairedEnd a
class Sorted a

instance {-# OVERLAPPABLE #-} DeDuped a => DeDuped (a :-> b)
instance {-# OVERLAPPABLE #-} (HasRG a, IsBam (a :-> b)) => HasRG (a :-> b)
instance {-# OVERLAPPABLE #-} PairedEnd a => PairedEnd (a :-> b)

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
          ,''IsTSV
          ,''IsVCF
          ,''PairedEnd
          ,''Sorted]

-- Tagging TH

allTransTagsPipe ty =
  forM allTags $ \tag -> do
    a <- newName "a"
    return $ InstanceD Nothing [AppT (ConT tag) (VarT a)] (AppT (ConT tag) (AppT (AppT (ConT ''(:->)) (VarT a)) (ConT ty) )) []

allTransTags ty =
  forM allTags $ \tag -> do
    a <- newName "a"
    return $ InstanceD Nothing [AppT (ConT tag) (VarT a)] (AppT (ConT tag) (AppT (ConT ty) (VarT a))) []
