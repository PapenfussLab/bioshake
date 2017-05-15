{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Core data types the pipeline abstraction.
module Bioshake.Types where

import           Bioshake.Implicit
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Strict
import qualified Data.Set                         as S
import           Data.String
import           Development.Shake

-- | Pipes output of phase a into phase b, i.e., forms a pipeline.
data a :-> b where (:->) :: a -> b -> a :-> b
infixl 1 :->

-- | Buildable abstracts things that can be turned into shake 'Action's.
class Buildable a where
  build :: Implicit Resource => a -> Action ()

-- | The compiler tracks the set of output files to ensure duplicate 'Rules' are
-- not generated. This allows multiple potentially overlapping pipelines to be
-- compiled down to a set of unique 'Rules'.
type Compiler = StateT (S.Set [FilePath]) Rules

-- | Compile pipelines to 'Rules'.
compileRules :: Compiler () -> Rules ()
compileRules p = evalStateT p mempty

-- | Pipelines are 'Compilable' when they can be compiled down to a set of
-- 'Rules' that build a list of output paths.
class Compilable a where
  compile :: Implicit Resource => a -> Compiler ()
  compile = return $ return mempty

-- | A pipeline @a :-> b@ is 'Compilable' to 'Rules' if @a@ is 'Compilable' and
-- we can generate an 'Action' to build @a:->b@ (i.e., @a:->b@ is 'Buildable').
-- This generates a 'Rules' which builds the 'paths' of @a:->b@ and 'need's the
-- 'paths' of @a@.
instance (Pathable a, Pathable (a :-> b), Compilable a, Buildable (a :-> b)) => Compilable (a :-> b) where
  compile pipe@(a :-> b) = do
    let outs = paths pipe
    set <- get
    when (outs `S.notMember` set) $ do
      lift $ outs &%> \_ -> do
        need (paths a)
        build pipe
      put (outs `S.insert` set)
    compile a

-- | Things are pathable if they can be mapped to a list of file paths. This is
-- used to make the files for a phase concrete in the build system.
class Pathable a where
  paths :: a -> [FilePath]

instance (Show a, Show b) => Show (a :-> b) where show (a :-> b) = show a ++ " :-> " ++ show b

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
      parse _   = error "cannot parse nucleotide"

-- For threaded config
data Threads = Threads Int deriving Show
