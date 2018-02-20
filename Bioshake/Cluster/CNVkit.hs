{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cluster.CNVkit where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.CNVkit
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

batchWGS :: (Given Config, Show a, Pathable a) => a -> BatchWGS Config
batchWGS = BatchWGS given

instance (Pathable a, Show a, Referenced a, IsBam a) => Buildable (a :-> BatchWGS Config) where
  build p@(a :-> b@(BatchWGS c _)) =
    let outs = paths p
    in withSubmit (buildBatchWGS (getCPUs c) b a outs) [Left c]
