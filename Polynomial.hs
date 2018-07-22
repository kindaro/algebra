{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Polynomial where

import Prelude hiding ((+), (*), (*>))
import qualified Prelude
import Data.Align
import Data.Monoid

newtype Polynomial a = Polynomial { unPolynomial :: [a] }

pattern P x = Polynomial x

unP = unPolynomial

class Ring a where
    (+), (*) :: a -> a -> a
    infixl 6 +
    infixl 7 *

instance Num a => Ring (Polynomial a) where
    (Polynomial xs) + (Polynomial ys)
        = Polynomial $ getSum <$> salign (Sum <$> xs) (Sum <$> ys)

    p@ (Polynomial (x:xs)) * q@ (Polynomial ys)
        = Polynomial ((x Prelude.*) <$> ys) + Polynomial (0: unPolynomial (Polynomial xs * Polynomial ys))
    (Polynomial [ ]) * _ = Polynomial [ ]

-- |
-- λ Polynomial [1, 1] * Polynomial [1, 1] * Polynomial [1, 1]
-- Polynomial {unPolynomial = [1,3,3,1]}

class VectorSpace k v where
    (*>) :: k -> v -> v

instance (Integral n, Num x) => VectorSpace n x where
    n *> x = fromIntegral n Prelude.* x

y 1 = Polynomial [0]
y i | i < 0 = error "TODO"
    | otherwise = Polynomial [1 - i, 1] * Polynomial [-i, 1] + y (i - 1)

instance (Show a, Eq a, Num a) => Show (Polynomial a) where

    show (Polynomial [ ]) = "0"

    show (Polynomial coefs) = unwords $ showOne <$> coefs
      where
        showOne :: a -> String
        showOne 0 = "+ _"
        showOne x | signum x == 1 = "+" ++ show (abs x)
                  | otherwise     = "-" ++ show (abs x)

computeAt :: forall a b. (Integral a, Num b) => Polynomial a -> b -> b
computeAt (Polynomial coefs) x = sum $ zipWith computeOne coefs [0..]
  where
    computeOne :: a -> Integer -> b
    computeOne coef power = coef *> (x ^ power)

(@@) = computeAt
