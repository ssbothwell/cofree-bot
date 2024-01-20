module Data.Align2
  ( Semialign2 (..),
  )
where

--------------------------------------------------------------------------------

import Data.Bifunctor
import Data.These (These (..))

--------------------------------------------------------------------------------

class Bifunctor p => Semialign2 p where
  {-# MINIMAL (align2 | alignWith2) #-}
  align2 :: p a b -> p c d -> p (These a c) (These b d)
  align2 = alignWith2 id id

  alignWith2 :: (These a c -> e) -> (These b d -> f) -> p a b -> p c d -> p e f
  alignWith2 f g ab cd = bimap f g $ align2 ab cd

instance Semialign2 (,) where
  align2 :: (a, b) -> (c, d) -> (These a c, These b d)
  align2 (a, b) (c, d) = (These a c, These b d)

instance Semialign2 Either where
  align2 :: Either a b -> Either c d -> Either (These a c) (These b d)
  align2 ab cd = case (ab, cd) of
    (Left a, Left c) -> Left (These a c)
    -- Note: Arbitrary left bias in these two case:
    (Left a, Right _d) -> Left (This a)
    (Right _b, Left c) -> Left (That c)
    (Right b, Right d) -> Right (These b d)
