{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Internal.Gridss where

import Bioshake
import Development.Shake
import Development.Shake.FilePath
import Data.Maybe
import System.IO.Temp

data Call = Call { jar :: FilePath
                 , threads :: Int
                 , resource :: Maybe Resource}

data ToBEDpe = ToBEDpe FilePath

instance Pathable a => Pathable (a :-> Call) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "gridss.vcf"]

instance Pathable a => Pathable (a :-> ToBEDpe) where
  paths (a :-> _) = ["tmp" </> concatMap takeFileName (paths a) <.> "gridss" <.> status <.> "bedpe" | status <- ["unfilt", "filt"]]

call jar = Call jar 0 Nothing
toBEDpe = ToBEDpe

instance Pathable a => IsVCF (a :-> Call)
instance Pathable a => IsBed (a :-> ToBEDpe)
