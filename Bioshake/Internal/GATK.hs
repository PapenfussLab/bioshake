{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.GATK where

import           Bioshake
import           Bioshake.TH
import qualified Control.Exception          as E
import           Control.Monad.Catch        (MonadMask (..))
import           Control.Monad.Trans
import           Development.Shake
import           Development.Shake.FilePath

data ReQual c = ReQual c FilePath FilePath

buildReQual (ReQual _ jar sites) a@(paths -> [input]) [out] =
    withTempDirectory' "." "gatk" $ \tmpDir -> do
      let tmpFile = tmpDir </> "recal.grp"
      () <- run "java"
        ["-jar", jar]
        ["-T",  "BaseRecalibrator"]
        ["-R", getRef a]
        ["-I", input]
        ["-knownSites", sites]
        ["-o", tmpFile]
      () <- run "java"
        ["-jar", jar]
        ["-T",  "PrintReads"]
        ["-R", getRef a]
        ["-I", input]
        ["-BQSR", tmpFile]
        ["-o", out]
      return ()

$(makeSingleTypes ''ReQual [''IsBam] [''IsSorted])

data RealignIndels c = RealignIndels c FilePath

buildRealignIndels (RealignIndels _ jar) a@(paths -> [input]) [out] =
    withTempDirectory' "." "gatk" $ \tmpDir -> do
      let list = tmpDir </> "list"
          realigned = tmpDir </> "realigned.bam"
      () <- run "java"
        ["-jar", jar]
        ["-T",  "RealignerTargetCreator"]
        ["-R", getRef a]
        ["-I", input]
        ["-o", list]
      () <- run "java"
        ["-jar", jar]
        ["-T",  "IndelRealigner"]
        ["-R", getRef a]
        ["-I", input]
        ["-targetIntervals", list]
        ["-o", out]
      return ()

$(makeSingleTypes ''RealignIndels [''IsBam] [''IsSorted])
