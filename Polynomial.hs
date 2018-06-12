module Polynomial where

import Prelude hiding ((+), (*))
import qualified Prelude
import Data.Align
import Data.Monoid

newtype Polynomial = Polynomial { unPolynomial :: [Integer] }

class Field a where
    (+), (*) :: a -> a -> a
    infixl 6 +
    infixl 7 *

instance Field Polynomial where
    (Polynomial xs) + (Polynomial ys)
        = Polynomial $ getSum <$> salign (Sum <$> xs) (Sum <$> ys)

    p@ (Polynomial (x:xs)) * q@ (Polynomial ys)
        = Polynomial ((x Prelude.*) <$> ys) + Polynomial (0: unPolynomial (Polynomial xs * Polynomial ys))
    (Polynomial [ ]) * _ = Polynomial [ ]

-- |
-- λ Polynomial [1, 1] * Polynomial [1, 1] * Polynomial [1, 1]
-- Polynomial {unPolynomial = [1,3,3,1]}

y 1 = Polynomial [0]
y i | i < 0 = error "TODO"
    | otherwise = Polynomial [1 - i, 1] * Polynomial [-i, 1] + y (i - 1)

instance Show Polynomial where
    show (Polynomial (x:xs)) = show (Polynomial xs) ++ " + " ++ show x  -- Not that easy.
        -- What about "x^2 + x + n"?
    show (Polynomial [ ]) = "0"


