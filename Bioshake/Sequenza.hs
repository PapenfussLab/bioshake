{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Sequenza where

import           Bioshake
import           Bioshake.Internal.Sequenza
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleThread ''Pileup2Seqz [''IsMPileup, ''GC] 'buildPileup2Seqz)
$(makeSingleThread ''Bin [''IsSeqzGZ] 'buildBin)
