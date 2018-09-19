{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Bioshake.Internal.CNVkit where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files         (createLink, rename)

data BatchOpts where
  DropLowCoverage :: BatchOpts

instance Show BatchOpts where
  show DropLowCoverage = "--drop-low-coverage"

data BatchWGS c where
  BatchWGS :: (Show a, Pathable a) => c -> [a] -> [BatchOpts] -> BatchWGS c
deriving instance Show c => Show (BatchWGS c)

data Batch c where
  Batch :: (Show a, Pathable a) => c -> [a] -> [BatchOpts] -> Batch c
deriving instance Show c => Show (Batch c)

class IsCNVkit c

buildBatchWGS t (BatchWGS _ norms opts) a [out] = do
  let inputs = paths a
      normPaths = concatMap paths norms
  lift $ need normPaths
  withTempDirectory' "tmp" "cnvkit" $ \tmpDir -> do
    () <- run "cnvkit.py batch"
      inputs
      (if length normPaths > 0 then "-n" : normPaths else [])
      "-m wgs"
      ["-f", getRef a]
      ["--annotate", annotations a]
      ["-p", show t]
      ["-d", tmpDir]
      (map show opts)
    () <- run "sleep 5"
    run "tar"
      ["-C", tmpDir]
      ["-cf", out]
      "."

$(makeSingleTypes ''BatchWGS [''IsCNVkit] [])

buildBatch t (Batch _ norms opts) a [out] = do
  let inputs = paths a
      normPaths = concatMap paths norms
  lift $ need normPaths
  withTempDirectory' "tmp" "cnvkit" $ \tmpDir -> do
    () <- run "cnvkit.py batch"
      inputs
      (if length normPaths > 0 then "-n" : normPaths else [])
      ["--targets", getBED a]
      ["-f", getRef a]
      ["--annotate", annotations a]
      ["-p", show t]
      ["-d", tmpDir]
      (map show opts)
    () <- run "sleep 5"
    run "tar"
      ["-C", tmpDir]
      ["-cf", out]
      "."

$(makeSingleTypes ''Batch [''IsCNVkit] [])
