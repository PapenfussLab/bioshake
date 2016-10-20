{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Internal.Cutadapt where

import Bioshake
import Development.Shake.FilePath

data Trim c = Trim c Seq

instance Pathable a => Pathable (a :-> Trim c) where
  paths (a :-> _) = map (\p -> "tmp" </> takeFileName p <.> "trimmed.fastq.gz") $ paths a

instance Pathable a => IsFastQ (a :-> Trim c)
