{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.Cutadapt where

import           Bioshake
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

data Trim c = Trim c Seq

buildTrim (Trim _ three') (paths -> inputs) outs =
  case inputs of
    [input] ->
      let [out] = outs in
      run "cutadapt"
        ["-a", show three']
        ["-o", out]
        [input]
    [_, _] ->
      let [o1, o2] = outs in
      run "cutadapt"
        ["-a", show three']
        ["-o", o1]
        ["-p", o2]
        inputs
    _ -> error "cutadapt: expecting either single or paired read sets"

$(makeMultiTypes ''Trim [''IsFastQ] [])
