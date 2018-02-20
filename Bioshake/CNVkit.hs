{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.CNVkit where

import           Bioshake
import           Bioshake.Internal.CNVkit
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

batchWGS :: (Given Threads, Show a, Pathable a) => a -> BatchWGS Threads
batchWGS = BatchWGS given

instance (Pathable a, Show a, Referenced a, IsBam a) => Buildable (a :-> BatchWGS Threads) where
  build p@(a :-> b@(BatchWGS (Threads t) _)) =
    let outs = paths p
    in withCmd t $ buildBatchWGS t b a outs
