{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Polynomial where

import Prelude hiding ((+), (*), (*>))
import qualified Prelude
import Data.Align
import Data.Monoid

newtype Polynomial a = Polynomial { unPolynomial :: [a] }

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

instance Show a => Show (Polynomial a) where
    show (Polynomial (x:xs)) = show (Polynomial xs) ++ " + " ++ show x  -- Not that easy.
        -- What about "x^2 + x + n"?
    show (Polynomial [ ]) = "0"

computeAt :: forall a b. (Integral a, Num b) => Polynomial a -> b -> b
computeAt (Polynomial coefs) x = sum $ zipWith computeOne coefs [0..]
  where
    computeOne :: a -> Integer -> b
    computeOne coef power = coef *> (x ^ power)

(@@) = computeAt
