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
  run "bedtools intersect"
    ["-a", input]
    ["-b", bed]
    "-header"
    [">", out]

$(makeSingleTypes ''FilterCapture [''IsVCF, ''CaptureOnly] [])

data FilterCaptureBam c = FilterCaptureBam c deriving Show

buildBedtoolsCaptureBam _ a@(paths -> [input]) [out] = do
  let bed = getBED a
  lift $ need [bed]
  run "bedtools intersect"
    ["-a", input]
    ["-b", bed]
    [">", out]

$(makeSingleTypes ''FilterCaptureBam [''IsBam, ''CaptureOnly] [])
