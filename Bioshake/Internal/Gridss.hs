{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.Gridss where

import           Bioshake
import           Bioshake.TH
import           Control.Monad.Trans        (lift)
import           Data.List                  (nub)
import           Development.Shake
import           Development.Shake.FilePath

data ComputeSamTags c = ComputeSamTags c deriving Show

buildComputeSamTags _ a@(paths -> [input]) [out] = do
  let mem = 31
      ref = getRef a
  lift . need $ (ref <.> "fai") : [ref <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]]
  withTempDirectory' "tmp" "gridss" $ \tmpDir ->
    memLimit mem $
      run "gridss" (concat ["-Xmx", show mem, "g"])
        ["--", "ComputeSamTags"]
        ["TMP_DIR=", tmpDir]
        ["REFERENCE_SEQUENCE=", ref]
        ["INPUT=", input]
        ["OUTPUT=", out]
        "ASSUME_SORTED=TRUE"
        "WORKING_DIR=tmp/"

$(makeSingleTypes ''ComputeSamTags [''IsBam] [''NameSorted])

data SoftClipsToSplitReads c = SoftClipsToSplitReads c deriving Show

buildSoftClipsToSplitReads t _ a@(paths -> [input]) [out] = do
  let mem = 31
      ref = getRef a
  lift . need $ (ref <.> "fai") : [ref <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]]
  withTempDirectory' "tmp" "gridss" $ \tmpDir ->
    memLimit mem $
      run "gridss" (concat ["-Xmx", show mem, "g"])
        ["--", "SoftClipsToSplitReads"]
        ["TMP_DIR=", tmpDir]
        ["REFERENCE_SEQUENCE=", ref]
        ["INPUT=", input]
        ["OUTPUT=", out]
        ["WORKER_THREADS=", show t]
        "WORKING_DIR=tmp/"

$(makeSingleTypes ''SoftClipsToSplitReads [''IsBam] [''Sorted, ''NameSorted])

data CollectMetrics c = CollectMetrics c deriving Show

buildCollectMetrics _ a@(paths -> [input]) outs = do
  let mem = 31
      ref = getRef a
      [out] = nub $ map dropExtension outs
  lift . need $ (ref <.> "fai") : [ref <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]]
  memLimit mem $
    run "gridss" (concat ["-Xmx", show mem, "g"])
      ["--", "analysis.CollectGridssMetrics"]
      ["REFERENCE_SEQUENCE=", ref]
      ["INPUT=", input]
      ["OUTPUT=", out]
      "THRESHOLD_COVERAGE=10000"

instance (Pathable a, Show a) => Pathable (a :-> CollectMetrics c) where
  paths (a :-> _) = let [p] = paths a in ["tmp" </> takeFileName p <.> ext ++ "_metrics" | ext <- ["idsv"
                                                                                                  ,"cigar"
                                                                                                  ,"tag"]]

class GridssMetrics c where
  metricBams :: c -> [FilePath]
instance Pathable a => GridssMetrics (a :-> CollectMetrics c) where
  metricBams (a :-> _) = paths a
instance GridssMetrics a => GridssMetrics (All a) where
  metricBams (All as) = concatMap metricBams as
instance Sorted a => Sorted (a :-> CollectMetrics c)
instance DupsMarked a => DupsMarked (a :-> CollectMetrics c)

class GridssAssembly a where
  assemblyInputs :: a -> [FilePath]
  assembly :: a -> FilePath

instance {-# OVERLAPPING #-} (Show a, Show c, Pathable a, GridssMetrics a) => GridssAssembly (a :-> AssembleBreakends c) where
  assemblyInputs (a :-> _) = metricBams a
  assembly a = head $ paths a

instance {-# OVERLAPPING #-} GridssAssembly a => GridssAssembly (a :-> b) where
  assemblyInputs (a :-> _) = assemblyInputs a
  assembly (a :-> _) = assembly a

data AssembleBreakends c = AssembleBreakends c deriving Show

buildAssembleBreakends t _ a [out] = do
  let mem = 31
      ref = getRef a
      inputs = metricBams a
  lift . need $ (ref <.> "fai") : [ref <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]]
  lift $ need inputs
  lift $ need [input <.> "bai" | input <- inputs]
  withTempDirectory' "tmp" "gridss" $ \tmpDir ->
    memLimit mem $
      run "gridss" (concat ["-Xmx", show mem, "g"])
        ["--", "AssembleBreakends"]
        ["TMP_DIR=", tmpDir]
        ["REFERENCE_SEQUENCE=", ref]
        (concatMap (\i -> ["INPUT=", i]) inputs)
        ["OUTPUT=", out]
        ["WORKER_THREADS=", show t]
        "WORKING_DIR=tmp/"

$(makeSingleTypes ''AssembleBreakends [''IsBam] [])

data IdentifyVariants c = IdentifyVariants c deriving Show

buildIdentifyVariants t _ a [out] = do
  let mem = 31
      ref = getRef a
      assInputs = assemblyInputs a
      [input] = metricBams a
  lift . need $ (ref <.> "fai") : [ref <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]]
  lift $ need assInputs
  lift $ need [input <.> "bai"]
  withTempDirectory' "tmp" "gridss" $ \tmpDir ->
    memLimit mem $
      run "gridss" (concat ["-Xmx", show mem, "g"])
        ["--", "IdentifyVariants"]
        ["TMP_DIR=", tmpDir]
        ["REFERENCE_SEQUENCE=", ref]
        (concatMap (\i -> ["INPUT=", i]) assInputs)
        ["ASSEMBLY=", input]
        ["OUTPUT_VCF=", out]
        ["WORKER_THREADS=", show t]
        "WORKING_DIR=tmp/"

$(makeSingleTypes ''IdentifyVariants [''IsVCF] [])

data AnnotateVariants c = AnnotateVariants c deriving Show

buildAnnotateVariants t _ a@(paths -> [input]) [out] = do
  let mem = 31
      ref = getRef a
      assInputs = assemblyInputs a
  lift . need $ (ref <.> "fai") : [ref <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]]
  lift $ need assInputs
  withTempDirectory' "tmp" "gridss" $ \tmpDir ->
    memLimit mem $
      run "gridss" (concat ["-Xmx", show mem, "g"])
        ["--", "AnnotateVariants"]
        ["TMP_DIR=", tmpDir]
        ["REFERENCE_SEQUENCE=", ref]
        (concatMap (\i -> ["INPUT=", i]) assInputs)
        ["INPUT_VCF=", input]
        ["ASSEMBLY=", assembly a]
        ["OUTPUT_VCF=", out]
        ["WORKER_THREADS=", show t]
        "WORKING_DIR=tmp/"

$(makeSingleTypes ''AnnotateVariants [''IsVCF] [])

data Call c = Call c deriving Show

instance (Show a, Pathable a) => Pathable (a :-> Call c) where
  paths (a :-> _) = [hashPath (paths a, show a) <.> "gridss" <.> "vcf"
                    ,hashPath (paths a, show a) <.> "gridss" <.> "bed"]

data Variants = Variants deriving Show

instance (Show a, Pathable a) => Pathable (a :-> Call c :-> Variants) where
  paths (a :-> _) = [head $ paths a]

instance {-# OVERLAPS #-} Compilable a => Compilable (a :-> Variants) where
  compile (a :-> _) = compile a

instance IsVCF (a :-> Variants)

data Assemblies = Assemblies deriving Show

instance (Show a, Pathable a) => Pathable (a :-> Call c :-> Assemblies) where
  paths (a :-> _) = [head . tail $ paths a]

instance {-# OVERLAPS #-} Compilable a => Compilable (a :-> Assemblies) where
  compile (a :-> _) = compile a

instance IsBed (a :-> Assemblies)

buildCall t _ a@(paths -> inputs) [vcf, ass] = do
  let mem = 31
      ref = getRef a
  lift . need $ (ref <.> "fai") : [ref <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]]
  withTempDirectory' "tmp" "gridss" $ \tmpDir ->
    memLimit mem $
      run "gridss" (concat ["-Xmx", show mem, "g"])
        ["--", "CallVariants"]
        ["TMP_DIR=", tmpDir]
        ["REFERENCE_SEQUENCE=", ref]
        (concatMap (\i -> ["INPUT=", i]) inputs)
        ["OUTPUT=", vcf]
        ["ASSEMBLY=", ass]
        ["WORKER_THREADS=", show t]
        "WORKING_DIR=tmp/"
