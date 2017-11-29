{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Kallisto(quant
                        ,quantSingle
                        ,abundance
                        ,bootstrap
                        ,seed
                        ,fragmentLength
                        ,fragmentSD
                        ,indexRules) where

import           Bioshake
import           Bioshake.Internal.Kallisto
import           Bioshake.TH
import           Bioshake.Implicit
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeThreaded' ''Quant [''Referenced, ''IsFastQ, ''PairedEnd] 'buildKallisto)
$(makeThreaded' ''QuantSingle [''Referenced, ''IsFastQ] 'buildKallistoSingle)
$(makeSingleThread ''Abundance [''IsKal] 'buildAbundance)

quant :: (Implicit Threads, Implicit [QuantOpts]) => Quant Threads
quant = Quant param param

quantSingle :: (Implicit Threads, Implicit [QuantOpts]) => QuantSingle Threads
quantSingle = check $ QuantSingle param param
  where
    check a@(QuantSingle _ opts) =
      if hasFragmentLength opts && hasFragmentSD opts then
        a
      else
        error "Kallisto: single end reads require fragment length and standard deviation"

indexRules =
  "//*.idx" %> \out -> do
    let input = dropExtension out
    need [input]
    cmd "kallisto index"
      ["-i", out]
      input
