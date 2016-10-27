{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Bioshake.Internal.Platypus where

import           Bioshake
import           Bioshake.TH
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           System.Posix.Files           (createLink, rename)

data Call c = Call c

buildPlatypus _ a@(paths -> inputs) [out] = do
  let bais = map ( <.> "bai" ) inputs
  lift $ need bais
  withTempDirectory' "." "platypus" $ \dir -> do
    let n = length inputs
        inputs' = [dir </> show i <.> "bam" | i <- [1..n]]
        bais' = map ( <.> "bai" ) inputs'
        out' = dir </> "calls.vcf"
    liftIO $ zipWithM_ createLink inputs inputs'
    liftIO $ zipWithM_ createLink bais bais'
    () <- run "platypus callVariants"
      ["--bamFiles=" ++ intercalate "," inputs']
      ["--refFile=" ++ getRef a]
      ["--output=" ++ out']
    liftIO $ rename out' out

$(makeSingleTypes ''Call [''IsVCF] [])
