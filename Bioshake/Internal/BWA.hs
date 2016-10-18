{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Internal.BWA where

import Bioshake
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath

data Align = Align { threads :: Int
                   , resource :: Maybe Resource}

align = Align 1 Nothing

instance Pathable a => Pathable (a :-> Align) where
  paths (a :-> _) = ["tmp" </> intercalate "-" (map takeFileName $ paths a) <.> "bwa.sam"]
instance Pathable a => IsSam (a :-> Align)
