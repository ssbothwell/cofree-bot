{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Functor.HKD where

--------------------------------------------------------------------------------

import Control.Category.Tensor
import Data.Functor.Identity
import Data.Functor.Monoidal
import Data.Kind
import Data.These
import Data.Void
import Data.Text (Text)
import Control.Lens (Contravariant)
import Data.Functor.Contravariant

--------------------------------------------------------------------------------

type NAry :: (Type -> Type -> Type) -> Type -> (Type -> Type) -> [Type] -> Type
data NAry t i f xs where
  Nil :: i -> NAry t i f '[]
  Cons :: (f x) `t` (NAry t i f xs) -> NAry t i f (x ': xs)

exampleEither :: NAry Either Void Identity '[Int, Bool]
exampleEither = Cons (Left 1)

exampleEither' :: NAry Either Void Identity '[Int, Bool]
exampleEither' = Cons (Right (Cons (Left (Identity True))))

exampleThese' :: NAry These Void Identity '[Int, Bool]
exampleThese' = Cons (These (Identity 1) (Cons (This (Identity True))))

exampleThese'' :: NAry These Void Identity '[Int, Bool]
exampleThese'' = Cons (This (Identity 1))

naryCombine :: (Functor f, Monoidal (->) t1 i1 to io f) => NAry to io f xs -> f (NAry t1 i1 Identity xs)
naryCombine = \case
  Nil i -> fmap Nil $ introduce i
  Cons t -> fmap Cons $ combine $ gbimap (fmap Identity) naryCombine t
