{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.SomaticSniper where

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

buildSomaticSniper _ a@(paths -> [normal, tumour]) [out] = do
  run "bam-somaticsniper -Q 40 -G -L -F vcf"
    ["-f", getRef a]
    [tumour]
    [normal]
    [out]

$(makeSingleTypes ''CallSomatic [''IsVCF] [''Sorted])

