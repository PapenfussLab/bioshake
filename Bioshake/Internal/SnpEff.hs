{-# LANGUAGE FlexibleContexts  #-}
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

data Annotate c = Annotate c deriving Show
data DBNSFP c = DBNSFP c deriving Show

class SnpEffAnnotated c

buildAnnot _ a@(paths -> [input]) [out] = do
  pwd <- liftIO getCurrentDirectory
  run "snpeff" (name a)
    ["-dataDir", pwd </> "tmp"]
    input
    [">", out]

buildDBNSFP _ a@(paths -> [input]) [out] = do
  let db = dbnsfp a
  lift $ need [db, db <.> "tbi"]
  pwd <- liftIO getCurrentDirectory
  run "snpsift dbnsfp" ["-db", db] [input] [">", out]

$(makeSingleTypes ''Annotate [''IsVCF, ''SnpEffAnnotated] [])
$(makeSingleTypes ''DBNSFP [''IsVCF] [])
