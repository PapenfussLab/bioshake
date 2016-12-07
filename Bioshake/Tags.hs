{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Bioshake.Tags where

import           Bioshake.Types
import           Control.Monad
import           Language.Haskell.TH

-- Filetypes

class Pathable a => DeDuped a
class Pathable a => HasRG a
class Pathable a => IsBam a
class Pathable a => IsBcf a
class Pathable a => IsBed a
class Pathable a => IsCSV a
class Pathable a => IsFastQ a
class Pathable a => IsGff a
class Pathable a => IsMPileup a
class Pathable a => IsSam a
class Pathable a => IsTSV a
class Pathable a => IsVCF a
class Pathable a => PairedEnd a
class Pathable a => Sorted a

instance {-# OVERLAPPABLE #-} (Pathable (a :-> b), DeDuped a) => DeDuped (a :-> b)
instance {-# OVERLAPPABLE #-} (Pathable (a :-> b), HasRG a, IsBam (a :-> b)) => HasRG (a :-> b)
instance {-# OVERLAPPABLE #-} (Pathable (a :-> b), PairedEnd a) => PairedEnd (a :-> b)

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
