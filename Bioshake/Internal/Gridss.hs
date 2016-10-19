{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Internal.Gridss where

import Bioshake
import Bioshake.Implicit
import Development.Shake
import Development.Shake.FilePath
import Data.Maybe
import System.IO.Temp

data Call = Call Threads FilePath
data ToBEDpe = ToBEDpe FilePath

instance Pathable a => Pathable (a :-> Call) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "gridss.vcf"]

instance Pathable a => Pathable (a :-> ToBEDpe) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "gridss" <.> status <.> "bedpe" | status <- ["unfilt", "filt"]]

call :: Implicit_ Threads => FilePath -> Call
call = Call param_
toBEDpe = ToBEDpe

instance Pathable a => IsVCF (a :-> Call)
instance Pathable a => IsBed (a :-> ToBEDpe)
