{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Platypus where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files           (createLink, rename)

data Call c = Call c deriving Show

buildPlatypus t _ a@(paths -> inputs) [out] = do
  let bais = map ( <.> "bai" ) inputs
      fai = getRef a <.> "fai"
  lift . need $ fai:bais
  run "platypus callVariants"
    ["--bamFiles=" ++ intercalate "," inputs]
    ["--refFile=" ++ getRef a]
    ["--output=" ++ out]
    ["--nCPU=" ++ show t]

$(makeSingleTypes ''Call [''IsVCF] [])
