{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Cutadapt where

import           Bioshake
import           Bioshake.Internal.Cutadapt
import           Bioshake.TH
import           Development.Shake

$(makeSingleThread ''Trim [''IsFastQ] 'buildTrim)
{- $trim Removes the given adaptor 'Seq' from fastq files using cutadapt. -}
