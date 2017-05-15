{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.SnpEff where

import           Bioshake
import           Bioshake.Internal.SnpEff
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleThread ''Annotate [''Referenced, ''IsVCF] 'buildAnnot)
{- $Annotate Annotate using snpEFF -}
$(makeSingleThread ''DBNSFP [''Referenced, ''IsVCF, ''SnpEffAnnotated] 'buildDBNSFP)
{- $Annotate Annotate with DBNSFP using snpEFF. -}
