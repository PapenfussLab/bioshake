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

pileup :: (Pathable v, Show v, IsVCF v, Sorted v, NoContigs v) => v -> Pileup ()
pileup = Pileup ()

instance (Pathable a, IsBam a, Sorted a) => Buildable (a :-> Pileup ()) where
  build p@(a :-> b@(Pileup _ _)) =
    let outs = paths p
    in withCmd 1 (buildFacets b a outs)

$(makeSingleThread ''Sort [''IsVCF] 'buildSort)
$(makeSingleThread ''FilterContigs [''IsVCF] 'buildNoContigs)
