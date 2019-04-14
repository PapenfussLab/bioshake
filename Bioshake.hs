{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Bioshake is a small framework for specifying bioinformatics pipelines. The
-- goal is to specify stages in a forward chaining manner (as is natural for the
-- domain) while guaranteeing as much robustness as possible to errors such as
-- mismatched file types or other attributes. Almost everything is handled in
-- the type system, and pipelines are compiled down to "Development.Shake"
-- 'Rules' for actual execution.
module Bioshake( module Types
               , module Data.Reflection
               , module Tags
               , All(..)
               , On(..)
               , Referenced(..)
               , Capture(..)
               , ignoringIOErrors
               , withTempDirectory
               , bioshake
               , out
               , Out
               , split
               , withAll
               , withPair) where

import           Bioshake.Cluster.Torque
import           Bioshake.Tags                    as Tags
import           Bioshake.Types                   as Types
import qualified Control.Exception                as E
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Strict
import           Data.List
import           Data.Reflection                  (Given, give, given)
import qualified Data.Set                         as S
import           Data.String
import           Development.Shake
import           Language.Haskell.TH
import           System.Directory                 (copyFile,
                                                   removeDirectoryRecursive)
import           System.IO.Temp                   (createTempDirectory)


-- | Attaches a reference genome.
class Referenced a where
  -- | The path to the reference fasta file.
  getRef :: a -> FilePath

  -- | The short name, e.g., hg19.
  name :: a -> String

  -- | Path to dbNSFP for the genome
  dbnsfp :: a -> FilePath
  dbnsfp _ = error "dbNSFP not available"

  -- | Gene annotations
  annotations :: a -> FilePath
  annotations _ = error "annotations not available"

-- | References flows down the pipeline regardless of the stage
instance Referenced a => Referenced (a :-> b) where
  getRef (a :-> _) = getRef a
  name (a :-> _) = name a
  dbnsfp (a :-> _) = dbnsfp a
  annotations (a :-> _) = annotations a

-- | Asserts a capture region.
class Capture a where
  getBED :: a -> FilePath

instance Capture a => Capture (a :-> b) where
  getBED (a :-> _) = getBED a

-- Hard naming outputs
data Out = Out [FilePath] deriving Show

-- | Explicitly names an output product. Outputs are automatically named in the
-- temporary directory except for this special case: this is how you obtain the
-- artifacts you are specifically interested in.
out = Out

instance Pathable (a :-> Out) where
  paths (_ :-> Out outs) = outs

instance Pathable a => Buildable (a :-> Out) where
  build ((paths -> inputs) :-> Out outs) = zipWithM_ ((liftIO .) . copyFile) inputs outs

$(allTransTagsPipe ''Out)

-- |Datatype to represent fan-in combinations.

data All a where
  All :: (Functor f, Foldable f) => f a -> All a

-- |Fan-in style combinator. Takes a collection of combines their output paths
-- as input paths for the subsequent stage.
withAll :: (Functor f, Foldable f) => f a -> All a
withAll = All

-- | Explicitly construct a fan-in of exactly two items
withPair :: a -> a -> All a
withPair a b = All [a, b]

instance Compilable a => Compilable (All a) where
  compile (All as) = mapM_ compile as

instance Pathable a => Pathable (All a) where
  paths (All ps) = nub $ concatMap paths ps

-- |Fan-ins are 'Referenced' iff all items are consistently 'Referenced'. Problems are caught at runtime unfortunately.
instance Referenced a => Referenced (All a) where
  getRef (All as) =  foldl1 (\l r -> if l == r then l else error "cannot combine mixed references") $ fmap getRef as
  name (All as) =  foldl1 (\l r -> if l == r then l else error "cannot combine mixed references") $ fmap name as
  dbnsfp (All as) =  foldl1 (\l r -> if l == r then l else error "cannot combine mixed references") $ fmap dbnsfp as
  annotations (All as) =  foldl1 (\l r -> if l == r then l else error "cannot combine mixed references") $ fmap annotations as

-- |Fan-ins are a 'Capture' iff all items are consistent.
instance Capture a => Capture (All a) where
  getBED (All as) = foldl1 (\l r -> if l == r then l else error "cannot combine mixed captures") $ fmap getBED as

instance Show a => Show (All a) where
  show (All as) = foldl1 (\l r -> l ++ "," ++ r) $ fmap show as

$(allTransTags ''All)

-- |Datatype to split outputs

data On a = On a Int

instance Compilable a => Compilable (On a) where
  compile (On a _) = compile a

instance Pathable a => Pathable (On a) where
  paths (On a i) = [paths a !! i]

instance Referenced a => Referenced (On a) where
  getRef (On a _) = getRef a
  name (On a _) = name a
  dbnsfp (On a _) = dbnsfp a
  annotations (On a _) = annotations a

instance Capture a => Capture (On a) where
  getBED (On a _) = getBED a

instance Show a => Show (On a) where
  show (On a i) = "(" ++ show a ++ ")_" ++ show i

$(allTransTags ''On)

on :: Pathable a => a -> Int -> On a
on a i
  | i >= 0 && i < length (paths a) = On a i
  | otherwise = error "on: index out of bounds"

split :: Pathable a => a -> [On a]
split a = [on a i | i <- [0..n - 1]]
  where
    n = length $ paths a

-- | Entry point to bioshake. Like 'shakeArgs' but also takes a number of
-- threads to use.
bioshake :: Int -- ^ Number of threads
         -> ShakeOptions -- ^ Options to pass to 'shakeArgs'.
         -> (Given Resource => Rules ()) -> IO ()
bioshake n opts cont = shakeArgs opts{shakeThreads = n} $ do
  res <- newResource "cpus" n
  give res cont

-- | Creates a temporary directory under a target directory according to a
-- naming template. The directory is cleaned up after executing the action. This
-- differs from "Development.Shake"'s 'withTempDir' in that it takes a target
-- directory and template whereas "Development.Shake" uses /tmp. This is
-- generally more useful, as ./tmp is used as the target directory by convention
-- in BioShake.
withTempDirectory :: FilePath -- ^ Target directory under which the temporary directory is created
                  -> String -- ^ Template for the temporary directory name
                  -> (FilePath -> Action b) -- ^ Action to carry out
                  -> Action b
withTempDirectory targetDir template act = do
  path <- liftIO $ createTempDirectory targetDir template
  act path `actionFinally` (liftIO . ignoringIOErrors $ removeDirectoryRecursive path)

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `E.catch` (\e -> const (return ()) (e :: IOError))
