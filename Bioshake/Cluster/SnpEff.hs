{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Cluster.SnpEff where

import           Bioshake
import           Bioshake.Cluster.Torque
import           Bioshake.Internal.SnpEff
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleCluster ''Annotate [''Referenced, ''IsVCF] 'buildAnnot)
$(makeSingleCluster ''DBNSFP [''Referenced, ''IsVCF, ''SnpEffAnnotated] 'buildDBNSFP)
