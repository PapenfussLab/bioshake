{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Bioshake.BWA(align, Align(..)) where

import Bioshake
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath

data Align = Align { reference :: FilePath
                   , threads :: Int
                   , resource :: Maybe Resource}

align ref = Align ref 1 Nothing

instance Pathable a => Pathable (a :-> Align) where
  paths (a :-> _) = ["tmp" </> intercalate "-" (map takeFileName $ paths a) <.> "bwa.sam"]
instance Pathable a => IsSam (a :-> Align)

instance IsFastQ a => Buildable a Align where
  build params (paths -> inputs) [out] =
    let cmd' =
          cmd "bwa mem"
            ["-t", show (threads params)]
            [reference params]
            inputs
            (FileStdout out)
    in case resource params of
         Just res -> withResource res (threads params) cmd'
         Nothing -> cmd'
