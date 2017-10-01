{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Facets where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.Internal.Facets
import           Bioshake.TH
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleThread ''Pileup [''IsVCF, ''Sorted, ''NoContigs, ''HasBams] 'buildFacets)
$(makeSingleThread ''Sort [''IsVCF] 'buildSort)
$(makeSingleThread ''FilterContigs [''IsVCF] 'buildNoContigs)
