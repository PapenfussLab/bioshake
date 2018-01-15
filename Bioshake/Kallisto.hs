{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Kallisto(quant
                        ,quantWith
                        ,quantSingle
                        ,quantSingleWith
                        ,abundance
                        ,bootstrap
                        ,seed
                        ,fusion
                        ,fusions
                        ,fragmentLength
                        ,fragmentSD
                        ,indexRules) where

import           Bioshake
import           Bioshake.Internal.Kallisto
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeThreaded' ''Quant [''Referenced, ''IsFastQ, ''PairedEnd] 'buildKallisto)
$(makeThreaded' ''QuantSingle [''Referenced, ''IsFastQ] 'buildKallistoSingle)
$(makeSingleThread ''Abundance [''IsKal] 'buildAbundance)
$(makeSingleThread ''Fusions [''IsKal] 'buildFusions)

quant :: Given Threads => Quant Threads
quant = Quant given []

quantWith :: Given Threads => [QuantOpts] -> Quant Threads
quantWith = Quant given

quantSingle :: Given Threads => QuantSingle Threads
quantSingle = quantSingleWith []

quantSingleWith :: Given Threads => [QuantOpts] -> QuantSingle Threads
quantSingleWith cfg = check $ QuantSingle given cfg
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
