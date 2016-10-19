{-# LANGUAGE Rank2Types, MultiParamTypeClasses, ScopedTypeVariables, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Bioshake.Implicit where

import Unsafe.Coerce

class Default a where
  def :: a

class Implicit_ a where
  param_ :: a

newtype Param_ a b = Param_ (Implicit_ a => b)

setParam_ :: forall a b. a -> (Implicit_ a => b) -> b
setParam_ a f = unsafeCoerce (Param_ f :: Param_ a b) a
{-# INLINE setParam_ #-}

($~) :: forall a b. (Implicit_ a => b) -> a -> b
($~) f = unsafeCoerce (Param_ f :: Param_ a b)
infixl 1 $~
{-# INLINE ($~) #-}

instance Default a => Implicit_ a where param_ = def
