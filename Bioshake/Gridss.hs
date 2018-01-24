{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Bioshake.Gridss where

import           Bioshake
import           Bioshake.Internal.Gridss
import           Bioshake.TH
import           Development.Shake
import           System.Directory
import           System.IO.Temp

$(makeSingleThread ''ComputeSamTags [''IsBam, ''Referenced, ''NameSorted] 'buildComputeSamTags)
$(makeThreaded ''SoftClipsToSplitReads [''IsBam, ''Referenced] 'buildSoftClipsToSplitReads)
$(makeSingleThread ''CollectMetrics [''IsBam, ''Referenced] 'buildCollectMetrics)
$(makeThreaded ''AssembleBreakends [''GridssMetrics, ''Referenced, ''Sorted, ''DupsMarked] 'buildAssembleBreakends)
$(makeThreaded ''IdentifyVariants [''GridssMetrics, ''Referenced, ''GridssAssembly] 'buildIdentifyVariants)
$(makeThreaded ''AnnotateVariants [''IsVCF, ''Referenced, ''GridssAssembly] 'buildAnnotateVariants)
$(makeThreaded ''Call [''IsBam, ''Sorted, ''Referenced] 'buildCall)

variants = Variants
assemblies = Assemblies
