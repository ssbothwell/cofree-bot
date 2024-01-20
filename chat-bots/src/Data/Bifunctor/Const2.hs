{-# LANGUAGE StandaloneDeriving #-}

module Data.Bifunctor.Const2
  ( Const2 (..),
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (Applicative (..))
import Data.String (IsString)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

newtype Const2 a b c = Const2 a
  deriving stock (Show, Functor)
  deriving newtype (Generic, Semigroup, Monoid)

instance Monoid m => Applicative (Const2 m b) where
  pure :: Monoid m => a -> Const2 m b a
  pure _ = Const2 mempty

  liftA2 :: Monoid m => (a -> b1 -> c) -> Const2 m b a -> Const2 m b b1 -> Const2 m b c
  liftA2 _ (Const2 x) (Const2 y) = Const2 (x <> y)

deriving instance IsString a => IsString (Const2 a b c)
