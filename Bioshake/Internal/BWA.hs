{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.BWA where

import           Bioshake                   hiding (C)
import           Bioshake.TH
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           GHC.TypeLits

data BWAOpts where
  K        :: Int    -> BWAOpts
  BW       :: Int    -> BWAOpts
  D        :: Int    -> BWAOpts
  R        :: Double -> BWAOpts
  Y        :: Int    -> BWAOpts
  C        :: Int    -> BWAOpts
  DC       :: Double -> BWAOpts
  W        :: Int    -> BWAOpts
  M        :: Int    -> BWAOpts
  RG       :: String -> BWAOpts
  SoftClip :: BWAOpts

instance Show BWAOpts where
  show (K p)    = "-k" ++ show p
  show (BW p)   = "-w" ++ show p
  show (D p)    = "-d" ++ show p
  show (R p)    = "-r" ++ show p
  show (Y p)    = "-y" ++ show p
  show (C p)    = "-c" ++ show p
  show (DC p)   = "-D" ++ show p
  show (W p)    = "-W" ++ show p
  show (M p)    = "-m" ++ show p
  show (RG r)   = concat ["-R", "'@RG\\tID:", r, "\\tSM:", r, "'"]
  show SoftClip = "-Y"

k x  = if x > 0 then K x else error "BWA: failed k > 0"
bw x = if x > 0 then BW x else error "BWA: failed bw > 0"
d x  = if x > 0 then D x else error "BWA: failed d > 0"
r x  = if x > 0 then R x else error "BWA: failed r > 0"
y x  = if x > 0 then Y x else error "BWA: failed y > 0"
c x  = if x > 0 then C x else error "BWA: failed c > 0"
dc x = if x > 0 then DC x else error "BWA: failed dc > 0"
w x  = if x > 0 then W x else error "BWA: failed w > 0"
m x  = if x > 0 then M x else error "BWA: failed m > 0"
rg r = if r /= "" then RG r else error "BWA: require non-empty rg string"
softClip = SoftClip

data Align c = Align c [BWAOpts] deriving Show

buildBWA t (Align _ opts) a@(paths -> inputs) [out] =
  if (length inputs > 2 || length inputs == 0)
    then error "BWA: need 1 single-end read set or 2 paired-end read sets"
    else do
      lift $ need [getRef a <.> ext | ext <- ["amb", "ann", "bwt", "pac", "sa"]]
      run "bwa mem"
        ["-t", show t]
        [getRef a]
        inputs
        (map show opts)
        ">" out

$(makeSingleTypes ''Align [''IsSam, ''NameSorted] [])
