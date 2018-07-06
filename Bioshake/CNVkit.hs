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
batchWGS a = BatchWGS given a []

batchWGSWithOpts :: (Given Threads, Show a, Pathable a) => a -> [BatchOpts] -> BatchWGS Threads
batchWGSWithOpts = BatchWGS given

instance (Pathable a, Show a, Referenced a, IsBam a) => Buildable (a :-> BatchWGS Threads) where
  build p@(a :-> b@(BatchWGS (Threads t) _ _)) =
    let outs = paths p
    in withCmd t $ buildBatchWGS t b a outs

batch :: (Given Threads, Show a, Pathable a) => a -> Batch Threads
batch a = Batch given a []

batchWithOpts :: (Given Threads, Show a, Pathable a) => a -> [BatchOpts] -> Batch Threads
batchWithOpts = Batch given

instance (Pathable a, Show a, Referenced a, IsBam a, Capture a) => Buildable (a :-> Batch Threads) where
  build p@(a :-> b@(Batch (Threads t) _ _)) =
    let outs = paths p
    in withCmd t $ buildBatch t b a outs
