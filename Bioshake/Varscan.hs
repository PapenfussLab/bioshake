{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bioshake.Varscan where

import           Bioshake
import           Bioshake.Internal.Varscan
import           Bioshake.TH
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath

$(makeSingleThread ''CallSomatic [''IsMPileup] 'buildVarscan)
$(makeSingleThread ''CopyNumber [''IsMPileup] 'buildCopyNumber)

--callSomatic :: CallSomatic ()
--callSomatic = CallSomatic ()
--
--instance IsMPileup a => Buildable (a :-> CallSomatic ()) where
--  build pipe@(a :-> b) =
--    let outs = paths pipe in
--      withCmd 1 $ buildVarscan b a outs
