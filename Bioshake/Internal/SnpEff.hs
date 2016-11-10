{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.SnpEff where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory
import           System.Posix.Files         (createLink, rename)

data Annotate c = Annotate c

buildAnnot _ a@(paths -> [input]) [out] = do
  pwd <- liftIO getCurrentDirectory
  run "snpEff" (name a)
    ["-dataDir", pwd </> "tmp"]
    input
    [">", out]

$(makeSingleTypes ''Annotate [''IsVCF] [])
