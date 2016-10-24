{-# LANGUAGE MultiParamTypeClasses, TypeOperators, FlexibleContexts, GADTs, FlexibleInstances, UndecidableInstances, ViewPatterns, ScopedTypeVariables, Rank2Types #-}
module Bioshake where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad
import Data.String
import Development.Shake
import qualified Data.Set as S
import System.Directory (copyFile)
import Bioshake.Implicit

data a :-> b where (:->) :: Buildable a b => a -> b -> a :-> b
infixl 1 :->

class Buildable a b where
  build :: b -> a -> [FilePath] -> Action ()
  threads :: a -> b -> Int
  threads _ _ = 1

type Compiler = StateT (S.Set [FilePath]) Rules

compileRules :: (Implicit_ Resource, Compilable a, Foldable t) => t a -> Rules ()
compileRules pipes = evalStateT (mapM_ compile pipes) mempty

class Compilable a where
  compile :: Implicit_ Resource => a -> Compiler ()
  compile = return $ return mempty

instance (Buildable a b, Pathable a, Pathable (a :-> b), Compilable a) => Compilable (a :-> b) where
  compile pipe@(a :-> b) = do
    let outs = paths pipe
    set <- get
    when (outs `S.notMember` set) $ do
      lift $ outs &%> \_ -> do
        need (paths a)
        withResource param_ (threads a b) $ build b a outs
      put (outs `S.insert` set)
    compile a

class Pathable a where
  paths :: a -> [FilePath]

-- Nucleotides
data Nuc = A | C | G | T
  deriving (Eq, Show)
newtype Seq = Seq [Nuc]
  deriving Eq

instance Show Seq where
  show (Seq s) = concatMap show s

instance IsString Seq where
  fromString str = Seq $ map parse str
    where
      parse 'A' = A
      parse 'C' = C
      parse 'G' = G
      parse 'T' = T
      parse 'a' = A
      parse 'c' = C
      parse 'g' = G
      parse 't' = T
      parse _ = error "cannot parse nucleotide"

-- Filetypes

class Pathable a => IsFastQ a
class Pathable a => IsPairedEnd a
class Pathable a => IsSam a
class Pathable a => IsBam a
class Pathable a => IsBcf a
class Pathable a => IsSorted a
class Pathable a => IsVCF a
class Pathable a => IsGff a
class Pathable a => IsBed a

instance (Pathable (a :-> b), IsPairedEnd a) => IsPairedEnd (a :-> b)

-- Hard naming outputs
data Out = Out [FilePath]

out = Out

instance Pathable (a :-> Out) where
  paths (_ :-> Out outs) = outs

instance Pathable a => Buildable a Out where
  build _ (paths -> inputs) = zipWithM_ ((liftIO .) . copyFile) inputs

-- Referenced (for track reference genomes automatically)

class Referenced a where
  getRef :: a -> FilePath

instance {-# OVERLAPPABLE #-} Referenced a => Referenced (a :-> b) where
  getRef (a :-> _) = getRef a

-- For threaded config

data Threads = Threads Int
--instance Default Threads where def = Threads 1

bioshake :: Int -> ShakeOptions -> (Implicit_ Resource => Rules ()) -> IO ()
bioshake n opts cont = shakeArgs opts{shakeThreads = n} $ do
  res <- newResource "cpus" n
  cont $~ res
