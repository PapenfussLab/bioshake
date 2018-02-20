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

data BatchWGS c where
  BatchWGS :: (Show a, Pathable a) => c -> a -> BatchWGS c

deriving instance Show c => Show (BatchWGS c)

class IsCNVkit c

buildBatchWGS t (BatchWGS _ norms) a [out] = do
  let inputs = paths a
      normPaths = paths norms
  lift $ need normPaths
  withTempDirectory' "tmp" "cnvkit" $ \tmpDir -> do
    () <- run "cnvkit.py batch"
      inputs
      ("-n" : normPaths)
      "-m wgs"
      ["-f", getRef a]
      ["--annotate", annotations a]
      ["-p", show t]
      ["-d", tmpDir]
    () <- run "sleep 5"
    run "tar"
      ["-C", tmpDir]
      ["-cf", out]
      "."

$(makeSingleTypes ''BatchWGS [''IsCNVkit] [])
