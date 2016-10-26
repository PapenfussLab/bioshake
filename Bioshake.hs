{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
module Bioshake( module Types
               , module Implicit
               , module Tags
               , Referenced(..)
               , bioshake
               , out
               , withAll) where

import           Bioshake.Cluster.Torque
import           Bioshake.Implicit                as Implicit
import           Bioshake.Tags                    as Tags
import           Bioshake.Types                   as Types
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Strict
import           Data.List
import qualified Data.Set                         as S
import           Data.String
import           Development.Shake
import           Language.Haskell.TH
import           System.Directory                 (copyFile)

-- Referenced (for track reference genomes automatically)
class Referenced a where
  getRef :: a -> FilePath

instance {-# OVERLAPPABLE #-} Referenced a => Referenced (a :-> b) where
  getRef (a :-> _) = getRef a

-- Hard naming outputs
data Out = Out [FilePath]

out = Out

instance Pathable (a :-> Out) where
  paths (_ :-> Out outs) = outs

instance Pathable a => Buildable a Out where
  build _ (paths -> inputs) = zipWithM_ ((liftIO .) . copyFile) inputs

$(allTransTagsPipe ''Out)

-- Fan-in
data All a where
  All :: (Functor f, Foldable f) => f a -> All a

withAll :: (Functor f, Foldable f) => f a -> All a
withAll = All

instance Compilable a => Compilable (All a) where
  compile (All as) = mapM_ compile as

instance Pathable a => Pathable (All a) where
  paths (All ps) = concatMap paths ps

instance Referenced a => Referenced (All a) where
  getRef (All as) =  foldl1 (\l r -> if l == r then l else error "cannot combine mixed references") $ fmap getRef as

$(allTransTags ''All)

bioshake :: Int -> ShakeOptions -> (Implicit_ Resource => Rules ()) -> IO ()
bioshake n opts cont = shakeArgs opts{shakeThreads = n} $ do
  res <- newResource "cpus" n
  cont $~ res
