{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Octopus where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files         (createLink, rename)

data OctopusOpts where
  NormalSample :: String -> OctopusOpts
  Fast :: OctopusOpts
  VeryFast :: OctopusOpts

instance Show OctopusOpts where
  show (NormalSample x) = "--normal-sample " ++x
  show Fast = "--fast"
  show VeryFast = "--very-fast"

normalSample x = if length x == 0 then error "Octopus: need non-empty name for normal sample" else NormalSample x
fast = Fast
veryFast = VeryFast

data Call c = Call c [OctopusOpts] deriving Show

buildOctopus t (Call _ opts) a@(paths -> inputs) [out] = do
  let bais = map ( <.> "bai" ) inputs
      fai = getRef a <.> "fai"
  lift . need $ fai:bais
  run "octopus"
    ["--reads", unwords inputs]
    ["--reference", getRef a]
    ["--output", out]
    ["--threads", show t]
    (map show opts)

$(makeSingleTypes ''Call [''IsVCF] [])
