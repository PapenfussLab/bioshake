{-# LANGUAGE MultiParamTypeClasses, TypeOperators, FlexibleContexts, GADTs, FlexibleInstances, UndecidableInstances, ViewPatterns, ScopedTypeVariables #-}
module Bioshake where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad
import Data.String
import Development.Shake
import Development.Shake.FilePath
import qualified Data.Set as S
import System.Directory (copyFile)

data a :-> b where (:->) :: Buildable a b => a -> b -> a :-> b
infixl 5 :->

class Buildable a b where
  build :: b -> a -> [FilePath] -> Action ()

type Compiler = StateT (S.Set [FilePath]) Rules

compileRules :: (Compilable a, Foldable t) => t a -> Rules ()
compileRules pipes = evalStateT (mapM_ compile pipes) mempty

class Compilable a where
  compile :: a -> Compiler ()

instance (Buildable a b, Pathable a, Pathable (a :-> b), Compilable a) => Compilable (a :-> b) where
  compile pipe@(a :-> b) = do
    let outs = paths pipe
    set <- get
    when (outs `S.notMember` set) $ do
      lift $ outs &%> \_ -> do
        need (paths a)
        build b a outs
      put (outs `S.insert` set)
    compile a

debugPipeline :: (Buildable a b, Pathable a, Pathable (a :-> b)) => a :-> b -> IO ()
debugPipeline pipe@(a :-> b) = do
  let outs = paths pipe
  putStr $ show $ paths a
  putStr " |> "
  print outs

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
class Pathable a => IsSorted a
class Pathable a => IsVCF a

-- Hard naming outputs
data Out = Out [FilePath]

out = Out

instance Pathable (a :-> Out) where
  paths (a :-> Out outs) = outs

instance Pathable a => Buildable a Out where
  build _ (paths -> inputs) = zipWithM_ ((liftIO .) . copyFile) inputs
