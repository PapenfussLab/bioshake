{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Hisat2 where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files         (createLink, rename)

data Align c = Align c deriving Show

buildHisat2 t _ a [out] =
  run "hisat2"
    ["-x", getRef a]
    (case paths a of
      [input] -> ["-U", input]
      [i1, i2] -> ["-1", i1, "-2", i2]
      _ -> error "hisat2: need 1 single-end read set or 2 paired-end read sets")
    ["-S", out]
    ["-p", show t]

$(makeSingleTypes ''Align [''IsSam] [])
