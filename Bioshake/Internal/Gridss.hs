{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Internal.Gridss where

import Bioshake
import Bioshake.Implicit
import Development.Shake.FilePath

data Call c = Call c FilePath
data ToBEDpe c = ToBEDpe c FilePath

instance Pathable a => Pathable (a :-> Call c) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "gridss.vcf"]

instance Pathable a => Pathable (a :-> ToBEDpe c) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "gridss" <.> status <.> "bedpe" | status <- ["unfilt", "filt"]]

instance Pathable a => IsVCF (a :-> Call c)
instance Pathable a => IsBed (a :-> ToBEDpe c)

