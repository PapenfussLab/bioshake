{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Bedtools where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files         (createLink, rename)

data FilterCapture c = FilterCapture c deriving Show

buildBedtoolsCapture _ a@(paths -> [input]) [out] = do
  let bed = getBED a
  lift $ need [bed]
  withTempDirectory' "tmp" "bedtools" $ \tmpDir -> do
    let intersection = tmpDir </> "intersection.vcf"
    () <- run "bedtools intersect"
      ["-a", input]
      ["-b", bed]
      [">", intersection]
    inhdr <- liftIO $ unlines . filter (\(l:_) -> l == '#') . lines <$> readFile input
    intcnt <- liftIO $ readFile intersection
    liftIO $ writeFile out (inhdr ++ intcnt)

$(makeSingleTypes ''FilterCapture [''IsVCF, ''CaptureOnly] [])
