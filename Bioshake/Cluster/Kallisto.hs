{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.Kallisto(quant
                                ,quantSingle
                                ,bootstrap
                                ,seed
                                ,fragmentLength
                                ,fragmentSD
                                ,indexRules) where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Implicit
import           Bioshake.Internal.Kallisto
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeCluster' ''Quant [''Referenced, ''IsFastQ, ''PairedEnd] 'buildKallisto)
$(makeCluster' ''QuantSingle [''Referenced, ''IsFastQ] 'buildKallistoSingle)

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

indexRules :: Implicit Config => Rules ()
indexRules =
  "//*.idx" %> \out -> do
    let input = dropExtension out
    need [input]
    withSubmit
      (run "kallisto index"
        ["-i", out]
        input)
      [Left param]
