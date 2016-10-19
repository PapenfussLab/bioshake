{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Internal.Cutadapt where

import Bioshake
import Development.Shake.FilePath

data Trim = Trim Seq

trim = Trim

instance Pathable a => Pathable (a :-> Trim) where
  paths (a :-> _) = map (\p -> "tmp" </> takeFileName p <.> "trimmed.fastq.gz") $ paths a

instance Pathable a => IsFastQ (a :-> Trim)
