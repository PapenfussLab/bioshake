{-# LANGUAGE TemplateHaskell, TypeOperators, FlexibleContexts, UndecidableInstances #-}
module Bioshake.Tags where

import Control.Monad
import Bioshake.Types
import Language.Haskell.TH

-- Filetypes

class Pathable a => IsFastQ a
class Pathable a => IsPairedEnd a
class Pathable a => IsSam a
class Pathable a => IsBam a
class Pathable a => IsBcf a
class Pathable a => IsSorted a
class Pathable a => IsVCF a
class Pathable a => IsGff a
class Pathable a => IsBed a

instance (Pathable (a :-> b), IsPairedEnd a) => IsPairedEnd (a :-> b)

allTags = [''IsFastQ, ''IsPairedEnd, ''IsSam, ''IsBam, ''IsSorted, ''IsVCF
          , ''IsGff, ''IsBed]

-- Tagging TH

allTransTagsPipe ty =
  forM allTags $ \tag -> do
    a <- newName "a"
    return $ InstanceD Nothing [AppT (ConT tag) (VarT a)] (AppT (ConT tag) (AppT (AppT (ConT ''(:->)) (VarT a)) (ConT ty) )) []

allTransTags ty =
  forM allTags $ \tag -> do
    a <- newName "a"
    return $ InstanceD Nothing [AppT (ConT tag) (VarT a)] (AppT (ConT tag) (AppT (ConT ty) (VarT a))) []
