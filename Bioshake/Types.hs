{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Bioshake.Types where

import           Bioshake.Implicit
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Strict
import qualified Data.Set                         as S
import           Data.String
import           Development.Shake

data a :-> b where (:->) :: Buildable a b => a -> b -> a :-> b
infixl 1 :->

class Buildable a b where
  build :: Implicit_ Resource => b -> a -> [FilePath] -> Action ()

type Compiler = StateT (S.Set [FilePath]) Rules

compileRules :: Compiler () -> Rules ()
compileRules p = evalStateT p mempty

class Compilable a where
  compile :: Implicit_ Resource => a -> Compiler ()
  compile = return $ return mempty

instance (Pathable a, Pathable (a :-> b), Compilable a) => Compilable (a :-> b) where
  compile pipe@(a :-> b) = do
    let outs = paths pipe
    set <- get
    when (outs `S.notMember` set) $ do
      lift $ outs &%> \_ -> do
        need (paths a)
        build b a outs
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
      parse _   = error "cannot parse nucleotide"

-- For threaded config

data Threads = Threads Int

