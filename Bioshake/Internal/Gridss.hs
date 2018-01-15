{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.Gridss where

import           Bioshake
import           Bioshake.TH
import           Development.Shake.FilePath

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

buildCall t _ a@(paths -> inputs) [vcf, ass] =
  let mem = 31 in
  withTempDirectory' "tmp" "gridss" $ \tmpDir ->
    memLimit mem $
      run "gridss" (concat ["-Xmx", show mem, "g"])
        ["--", "CallVariants"]
        ["TMP_DIR=", tmpDir]
        ["WORKING_DIR=", tmpDir]
        ["REFERENCE_SEQUENCE=", getRef a]
        (concatMap (\i -> ["INPUT=", i]) inputs)
        ["OUTPUT=", vcf]
        ["ASSEMBLY=", ass]
        ["WORKER_THREADS=", show t]
