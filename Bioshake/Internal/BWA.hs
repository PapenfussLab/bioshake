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
import           Data.List
import           Development.Shake.FilePath

data Align c = Align c deriving Show

buildBWA t _ a@(paths -> inputs) [out] =
    run "bwa mem"
      ["-t", show t]
      [getRef a]
      inputs
      ">" out

$(makeSingleTypes ''Align [''IsSam] [])
