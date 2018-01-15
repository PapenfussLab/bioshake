{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.Kallisto(quant
                                ,quantWith
                                ,quantSingle
                                ,quantSingleWith
                                ,bootstrap
                                ,seed
                                ,fusion
                                ,fragmentLength
                                ,fragmentSD
                                ,indexRules) where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.Kallisto
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeCluster' ''Quant [''Referenced, ''IsFastQ, ''PairedEnd] 'buildKallisto)
$(makeCluster' ''QuantSingle [''Referenced, ''IsFastQ] 'buildKallistoSingle)

quant :: Given Config => Quant Config
quant = Quant given []

quantWith :: Given Config => [QuantOpts] -> Quant Config
quantWith = Quant given

quantSingle :: Given Config => QuantSingle Config
quantSingle = quantSingleWith []

quantSingleWith :: Given Config => [QuantOpts] -> QuantSingle Config
quantSingleWith cfg = check $ QuantSingle given cfg
  where
    check a@(QuantSingle _ opts) =
      if hasFragmentLength opts && hasFragmentSD opts then
        a
      else
        error "Kallisto: single end reads require fragment length and standard deviation"

indexRules :: Given Config => Rules ()
indexRules =
  "//*.idx" %> \out -> do
    let input = dropExtension out
    need [input]
    withSubmit
      (run "kallisto index"
        ["-i", out]
        input)
      [Left given]
