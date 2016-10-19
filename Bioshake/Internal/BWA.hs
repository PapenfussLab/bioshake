{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
module Bioshake.Internal.BWA where

import Bioshake
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Implicit

data Align = Align Threads

align :: Implicit_ Threads => Align
align = Align param_

instance Pathable a => Pathable (a :-> Align) where
  paths (a :-> _) = ["tmp" </> intercalate "-" (map takeFileName $ paths a) <.> "bwa.sam"]
instance Pathable a => IsSam (a :-> Align)
