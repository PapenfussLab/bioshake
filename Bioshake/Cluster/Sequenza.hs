{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.Sequenza (pileup2Seqz, bin, GC(..)) where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.Sequenza
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleCluster ''Pileup2Seqz [''IsMPileup, ''GC] 'buildPileup2Seqz)
$(makeSingleCluster ''Bin [''IsSeqzGZ] 'buildBin)
