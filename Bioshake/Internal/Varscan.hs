{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Varscan where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files         (createLink, rename)
import System.Directory (copyFile)

data CallSomatic c = CallSomatic c deriving Show
data CopyNumber c = CopyNumber c deriving Show

buildVarscan _ a@(paths -> [input]) [out] = do
  () <- run "varscan somatic"
    input
    out
    "--mpileup 1 --output-vcf 1"
  liftIO $ copyFile (out <.> "snp") out
  indels <- fmap (unlines . filter (\(c:_) -> c /= '#') . lines) . liftIO $ readFile (out <.> "indel")
  liftIO $ appendFile out indels

$(makeSingleTypes ''CallSomatic [''IsVCF] [])

buildCopyNumber _ a@(paths -> [input]) [out] = do
  () <- run "varscan copynumber"
    input
    out
    "--mpileup 1"
  liftIO $ copyFile (out <.> "snp") out
  indels <- fmap (unlines . filter (\(c:_) -> c /= '#') . lines) . liftIO $ readFile (out <.> "indel")
  liftIO $ appendFile out indels

class IsCNV a

$(makeSingleTypes ''CopyNumber [''IsCNV] [])
