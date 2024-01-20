{-# LANGUAGE GADTs #-}

module Data.Bifunctor.HKD where

--------------------------------------------------------------------------------

import Control.Category.Tensor
import Data.Bifunctor
import Data.Bifunctor.Monoidal
import Data.Kind
import Data.Profunctor

--------------------------------------------------------------------------------

-- | Maps a list of pairs of types using the given type constructor,
-- then folds the list using the provided monoidal structure on Types.
--
-- __Examples:__
--
-- >>> :{
--  let ex1 :: FoldMap These Void First ['((), Bool), '((), Text)]
--      ex1 = Cons $ This (First ())
-- :}
--
-- >>> :{
--  let ex2 :: FoldMap These Void First ['((), Bool), '((), Text)]
--      ex2 = Cons $ That $ Cons $ This $ First ()
--
-- >>> :{
--  let ex3 :: FoldMap These Void First ['((), Bool), '((), Text)]
--      ex3 = Cons $ These (First ()) $ Cons $ This $ First ()
-- :}
type FoldMap :: (Type -> Type -> Type) -> Type -> (Type -> Type -> Type) -> [(Type, Type)] -> Type
data FoldMap t i f xs where
  Nil :: {unNil :: i} -> FoldMap t i f '[]
  Cons :: {unCons :: (p x y) `t` (FoldMap t i p xs)} -> FoldMap t i p ('(x, y) ': xs)

data First a b = First {unFirst :: a}
  deriving Show

data Second a b = Second {unSecond :: b}
  deriving Show

sequenceFoldMapB :: (Bifunctor p, Monoidal (->) t1 i1 t2 i2 to io p) => FoldMap to io p xs -> p (FoldMap t1 i1 First xs) (FoldMap t2 i2 Second xs)
sequenceFoldMapB = \case
  Nil i -> bimap Nil Nil $ introduce i
  Cons t -> bimap Cons Cons $ combine $ gbimap (gbimap First Second) sequenceFoldMapB t

sequenceFoldMapP :: (Profunctor p, Monoidal (->) t1 i1 t2 i2 to io p) => FoldMap to io p xs -> p (FoldMap t1 i1 First xs) (FoldMap t2 i2 Second xs)
sequenceFoldMapP = \case
  Nil i -> dimap unNil Nil $ introduce i
  Cons t -> dimap unCons Cons $ combine $ gbimap (dimap unFirst Second) sequenceFoldMapP t
