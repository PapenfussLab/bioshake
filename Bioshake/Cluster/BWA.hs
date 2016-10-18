{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.Cluster.BWA(align, Align(..)) where

import Bioshake
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import Bioshake.Cluster.Torque

data Align = Align { threads :: Int
                   , maxMem :: Int
                   , queue :: Queue}

align = Align 20 (gb 10) "small"

instance Pathable a => Pathable (a :-> Align) where
  paths (a :-> _) = ["tmp" </> intercalate "-" (map takeFileName $ paths a) <.> "bwa.sam"]
instance Pathable a => IsSam (a :-> Align)

instance (Referenced a, IsFastQ a) => Buildable a Align where
  build params a@(paths -> inputs) [out] =
    submit "bwa mem"
      ["-t", show (threads params)]
      (CPUs (threads params))
      [getRef a]
      inputs
      [">", out]
      (Mem (maxMem params))
      (Queue (queue params))
