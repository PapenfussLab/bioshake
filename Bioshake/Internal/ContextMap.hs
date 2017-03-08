{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.ContextMap where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.TH
import           Control.Monad.Trans (lift)
import           Data.Maybe
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory

data Align c = Align c deriving Show

buildContextMap t _ a@(paths -> inputs) [out] = do
  path <- liftIO $ getSearchPath
  bwaBin <- liftIO $ fromMaybe (error "Cannot find BWA in path") <$> findFile path "bwa"
  withTempDirectory' "tmp" "contextmap" $ \tmpDir -> do
    () <- run "contextmap"
      ["-t", show t]
      ["-genome", dropFileName $ getRef a]
      ["-indices", getRef a]
      ["-reads", intercalate "," inputs]
      ["-aligner_name", "bwa"]
      ["-aligner_bin", bwaBin]
      ["-o", tmpDir]
    lift $ copyFile' (tmpDir </> "mapping.sam") out

$(makeSingleTypes ''Align [''IsSam] [])
