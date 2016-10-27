{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake.Internal.Gridss where

import           Bioshake
import           Bioshake.Implicit
import           Bioshake.TH
import           Development.Shake.FilePath

data Call c = Call c FilePath

buildCall t (Call _ jar) a@(paths -> inputs) [out] =
  let mem = t * 2 + 8 in
  withTempDirectory' "." "gridss" $ \tmpDir -> 
    memLimit mem $ 
      run "java -ea" (concat ["-Xmx", show mem, "g"])
        ["-cp", jar, "au.edu.wehi.idsv.Idsv"]
        ["TMP_DIR=", tmpDir]
        ["WORKING_DIR=", tmpDir]
        ["REFERENCE=", getRef a]
        (concat $ zipWith (\input ic -> ["INPUT=", input, "IC=", show ic]) inputs [1..])
        ["OUTPUT=", out]
        ["WORKER_THREADS=", show t]

$(makeSingleTypes ''Call [''IsVCF] [])
