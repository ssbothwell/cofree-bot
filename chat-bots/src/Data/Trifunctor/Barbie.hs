{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Trifunctor.Barbie where

-- ( First (..),
--   Second (..),
--   Optional (..),
--   traverseThese,
--   zipMyApp,
-- )

--------------------------------------------------------------------------------

import Data.Bifunctor.Monoidal
import Data.Bifunctor.Monoidal.Specialized
import Data.Bool
import Data.Chat.Bot (Bot)
import Data.Chat.Bot.Serialization (TextSerializer)
import Data.Chat.Bot.Serialization qualified as S
import Data.Chat.Utils (can)
import Data.Kind (Type)
import Data.Text (Text)
import Data.These
import Data.Bifunctor.HKD
import Data.Void
import Data.Profunctor
import Data.Bifunctor

--------------------------------------------------------------------------------

newtype Flip b l r = Flip {runFlip :: b r l}
  deriving (Eq, Ord, Read, Show)

instance Semigroupal (->) (,) (,) (,) f => Semigroupal (->) (,) (,) (,) (Flip f) where
  combine :: Semigroupal (->) (,) (,) (,) f => (Flip f x y, Flip f x' y') -> Flip f (x, x') (y, y')
  combine (Flip f1, Flip f2) = Flip $ combine (f1, f2)

instance Semigroupal (->) These These (,) (Flip TextSerializer) where
  combine :: (Flip TextSerializer x y, Flip TextSerializer x' y') -> Flip TextSerializer (These x x') (These y y')
  combine (Flip (S.Serializer par1 pri1), Flip (S.Serializer par2 pri2)) =
    Flip $
      S.Serializer
        { parser = uncurry can . (par1 &&& par2),
          printer = these pri1 pri2 (\y y' -> pri1 y <> pri2 y')
        }

--------------------------------------------------------------------------------

data HKD2 f = HKD2 {a :: f () Bool, b :: f () Text}

helloBot :: Monad m => Bot m s () Text
helloBot = undefined

coinFlipBot :: Monad m => Bot m s () Bool
coinFlipBot = undefined

-- | Packing the HKD with 'Bot' gives us the bot subroutinesf for our
-- HKD.
botHKD :: Monad m => HKD2 (Bot m s)
botHKD =
  HKD2
    { a = coinFlipBot,
      b = helloBot
    }

serializerHKD :: HKD2 (Flip TextSerializer)
serializerHKD =
  HKD2
    { a = Flip S.Serializer {parser = bool Nothing (Just ()) . (== "flip a coin"), printer = bool "tails" "heads"},
      b = Flip S.Serializer {parser = bool Nothing (Just ()) . (== "cofree-bot"), printer = id}
    }

--------------------------------------------------------------------------------

newtype Compose2 f g a b = Compose2 (f (g a b))
  deriving Functor

instance (Functor f, Bifunctor g) => Bifunctor (Compose2 f g) where
  bimap :: Bifunctor g => (a -> b) -> (c -> d) -> Compose2 f g a c -> Compose2 f g b d
  bimap f g (Compose2 fg) = Compose2 $ fmap (bimap f g) fg

instance (Functor f, Profunctor g) => Profunctor (Compose2 f g) where
  dimap :: (Functor f, Profunctor g) => (a -> b) -> (c -> d) -> Compose2 f g b c -> Compose2 f g a d
  dimap f g (Compose2 fg) = Compose2 $ fmap (dimap f g) fg

type Optional :: ((k -> k -> Type) -> Type) -> (k -> k -> Type) -> Type
newtype Optional hkd f = Optional (hkd (Compose2 Maybe f))

class HKD p t i hkd where
  type Fields hkd :: [(Type, Type)]

  to :: hkd f `p` FoldMap t i f (Fields hkd)
  from :: Profunctor f => FoldMap t i f (Fields hkd) `p` hkd f


instance HKD (->) (,) () HKD2 where
  type Fields HKD2 = ['((), Bool), '((), Text)]

  to :: HKD2 f -> FoldMap (,) () f (Fields HKD2)
  to HKD2 {..} = Cons (a, Cons (b, Nil ()))

  from :: FoldMap (,) () f (Fields HKD2) -> HKD2 f
  from = \case
    Cons (a, Cons (b, _)) -> HKD2 {..}

instance HKD (->) (,) () hkd => HKD (Star Maybe) These Void (Optional hkd) where
  type Fields (Optional hkd) = ['((), Bool), '((), Text)]

  to :: HKD (->) (,) () hkd => Star Maybe (Optional hkd f) (FoldMap These Void f (Fields (Optional hkd)))
  to = Star $ \(Optional hkd) -> _

  from :: (Profunctor f, HKD (->) (,) () hkd) => Star Maybe (FoldMap These Void f (Fields (Optional hkd))) (Optional hkd f)
  from = _ (sequenceFoldMapP @(Star Maybe) @These @Void @These @Void @These @Void)
