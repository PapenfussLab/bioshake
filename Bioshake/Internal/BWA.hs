{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.BWA where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.TH
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

data Align c = Align c deriving Show

buildBWA t _ a@(paths -> inputs) [out] = do
    lift $ need [getRef a <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]]
    run "bwa mem"
      ["-t", show t]
      [getRef a]
      inputs
      ">" out

$(makeSingleTypes ''Align [''IsSam] [])
