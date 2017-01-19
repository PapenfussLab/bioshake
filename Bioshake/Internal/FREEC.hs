{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.FREEC where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files           (createLink, rename)

data CNVExome c = CNVExome c

buildFREECExome t _ a@(paths -> [normal, tumour]) [out] =
  withTempDirectory' "tmp/" "FREEC" $ \tmp -> do
    let conf = tmp </> "config.cfg"
        confContents = unlines ["[general]"
                               ,"window = 0"
                               ,unwords ["outputDIR =", tmp]
                               ,"contaminationAdjustment = TRUE"
                               ,"ploidy=2,3,4"
                               ,"readCountThreshold = 50"
                               ,unwords ["chrLenFile =", lenFile]
                               ,unwords ["chrFiles =", dropFileName lenFile]

                               ,"[sample]"
                               ,unwords ["mateFile =", tumour]
                               ,"inputFormat = pileup"
                               ,"mateOrientation = FR"

                               ,"[control]"
                               ,unwords ["mateFile =", normal]
                               ,"inputFormat = pileup"
                               ,"mateOrientation = FR"

                               ,"[target]"
                               ,unwords ["captureRegions =", capture]
                               ]
        lenFile = getRef a <.> ".fai"
        capture = getBED a
    lift $ need [lenFile, capture]
    liftIO $ writeFile conf confContents
    () <- run "freec"
      ["-conf", conf]
    run "tar -zcvf" [out] ["-C", tmp] "."

$(makeSingleTypes ''CNVExome [''IsTGZ] [])
