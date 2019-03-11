{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Strelka where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory           (copyFile)
import           System.Posix.Files         (createLink, rename)

data CallSomatic c = CallSomatic c deriving Show

buildStrelkaSomatic t _ a@(paths -> [normal, tumour]) [snvs, indels] = do
  withTempDirectory' "tmp" "strelka" $ \tmpDir -> do
    () <- run "configureStrelkaSomaticWorkflow.py"
      ["--normalBam", normal]
      ["--tumourBam", tumour]
      ["--ref", getRef a]
      ["--runDir", tmpDir </> "strelka"]
    () <- run [tmpDir </> "strelka" </> "runWorkflow.py"]
      "-m local"
      ["-j", show t]
    () <- run "gunzip"
      ["<", tmpDir </> "strelka/results/variants/somatic.snvs.vcf.gz"]
      [">", snvs]
    run "gunzip"
      ["<", tmpDir </> "strelka/results/variants/somatic.indels.vcf.gz"]
      [">", indels]

instance (Show a, Show c, Pathable a) => Pathable (a :-> CallSomatic c) where
  paths (a :-> b) = [hashPath (paths a, show a, show b) <.> s <.> "Strelka" <.> "CallSomatic" <.> "vcf" | s <- ["snvs", "indels"]]
instance IsVCF (a :-> CallSomatic c)
