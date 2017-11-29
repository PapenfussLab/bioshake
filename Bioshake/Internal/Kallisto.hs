{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Kallisto where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files         (createLink, rename)

class IsKal c

data Quant c = Quant c [QuantOpts] deriving Show
data QuantSingle c = QuantSingle c [QuantOpts] deriving Show

data QuantOpts where
  Bootstrap :: Int -> QuantOpts
  Seed :: Int -> QuantOpts
  FragmentLength :: Double -> QuantOpts
  FragmentSD :: Double -> QuantOpts
  Single :: QuantOpts

instance Default [QuantOpts] where def = []

instance Show QuantOpts where
  show (Bootstrap n) = "--bootstrap-samples=" ++ show n
  show (Seed s) = "--seed=" ++ show s
  show (FragmentLength d) = "--fragment-length=" ++ show d
  show (FragmentSD d) = "--sd=" ++ show d
  show Single = "--single"

bootstrap x = if x > 1 then Bootstrap x else error "Kallisto: need positive number of bootstrap samples"
fragmentLength x = if x > 0 then FragmentLength x else error "Kallisto: fragment length must be positive"
fragmentSD x = if x > 0 then FragmentSD x else error "Kallisto: fragment SD must be positive"
seed = Seed

hasFragmentSD [] = False
hasFragmentSD (FragmentSD _ : _) = True
hasFragmentSD (x:xs) = hasFragmentSD xs

hasFragmentLength [] = False
hasFragmentLength (FragmentLength _ : _) = True
hasFragmentLength (x:xs) = hasFragmentLength xs

buildKallisto t (Quant _ opts) a@(paths -> inputs@[_,_]) [out] =
  let idx = getRef a <.> "idx" in
  buildKallisto' t opts idx inputs out

buildKallistoSingle t (QuantSingle _ opts) a@(paths -> [input]) [out] =
  let idx = getRef a <.> "idx" in
  buildKallisto' t (Single : opts) idx [input] out

buildKallisto' t opts idx inputs out = do
  lift $ need [idx]
  withTempDirectory' "tmp" "kallisto" $ \tmpDir -> do
    () <- run "kallisto quant"
      ["-i", idx]
      ["-o", tmpDir]
      ["-t", show t]
      (map show opts)
      inputs
    run "tar"
      ["-C", tmpDir]
      ["-cf", out]
      "."

$(makeSingleTypes ''Quant [''IsKal] [])
$(makeSingleTypes ''QuantSingle [''IsKal] [])

data Abundance c = Abundance c deriving Show

buildAbundance _ (paths -> [input]) [out] = do
  withTempDirectory' "tmp" "kallisto" $ \tmpDir -> do
    () <- run "tar"
      ["-C", tmpDir]
      ["-xf", input]
    lift $ copyFile' (tmpDir </> "abundance.tsv") out

$(makeSingleTypes ''Abundance [''IsTSV] [])
