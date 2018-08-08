{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Polynomial where

import Prelude
import qualified Prelude
import Data.Align
import Data.Monoid
import Text.PrettyPrint.Boxes
import Data.List (foldl')

-- $setup
-- λ :set -XAllowAmbiguousTypes
-- λ :set -XFlexibleContexts

-- | A type for polynomials, expressed as the list of coefficients, the highest power the last.
newtype Polynomial a = Polynomial { unPolynomial :: [a] }

pattern P :: [a] -> Polynomial a
pattern P x = Polynomial x

unP :: Polynomial a -> [a]
unP = unPolynomial

instance (Show a, Eq a, Num a) => Show (Polynomial a)
  where show = showWithVariable "x"

showWithVariable _ (Polynomial [ ]) = "0"

showWithVariable s (Polynomial coefs) = init . render . hsep 1 center1 . filter (\b -> cols b /= 0)
                        $ zipWith3 boxCoefAtPower
                            (reverse coefs)
                            [length coefs - 1, length coefs - 2 .. 0]
                            (True: repeat False)
  where
    boxCoefAtPower :: (Num a, Eq a, Show a) => a -> Int -> Bool -> Box
    boxCoefAtPower 0 _ True = "0"
    boxCoefAtPower 0 _ False = nullBox
    boxCoefAtPower c 0 isFirst = hcat center1
        [ if signum c == -1 then "- " else withDefaultNull isFirst "+ "
        , (text . show . abs $ c) ]

    boxCoefAtPower c p isFirst = hcat center1
        [ if signum c == -1 then " - " else withDefaultNull isFirst "+ "
        , withDefaultNull (abs c == 1) (text . show . abs $ c)
        , text s
        , withDefaultNull (p == 1) (moveUp 2 . text . show $ p) ]

    withDefaultNull :: Bool -> Box -> Box
    withDefaultNull True  _ = nullBox
    withDefaultNull False b = b

-- ^ Polynomials are displayed as usual.
--
-- λ P [0,1,2,3]
--   3     2
-- 3x  + 2x  + x
-- <BLANKLINE>
--
-- λ P [2,0,0,0,0,2]
--   5
-- 2x  + 2
-- <BLANKLINE>

class Ring a where
    (#+), (#*) :: a -> a -> a
    infixl 6 #+
    infixl 7 #*

    invert :: a -> a

instance Num a => Ring (Polynomial a) where
    (Polynomial xs) #+ (Polynomial ys)
        = Polynomial $ getSum <$> salign (Sum <$> xs) (Sum <$> ys)

    p@ (Polynomial (x:xs)) #* q@ (Polynomial ys)
        = Polynomial ((x *) <$> ys) #+ Polynomial (0: unPolynomial (Polynomial xs #* Polynomial ys))
    (Polynomial [ ]) #* _ = Polynomial [ ]

    invert (Polynomial xs) = Polynomial (negate <$> xs)

-- ^ Polynomials may be added:
--
-- λ P [1] #+ P [0,2]
-- 2x + 1
--
--   And multiplied:
--
-- λ Polynomial [1, 1] #* Polynomial [1, 1] #* Polynomial [1, 1]
--  3     2         
-- x  + 3x  + 3x + 1
-- <BLANKLINE>

class VectorSpace k v where
    (#>) :: k -> v -> v

instance (Integral n, Num x) => VectorSpace n x where
    n #> x = fromIntegral n Prelude.* x

instance {-# OVERLAPS #-} Num a => VectorSpace a (Polynomial a)
  where
    y #> (Polynomial xs) = Polynomial $ (Prelude.* y) <$> xs

-- ^ Polynomials may be multiplied by a scalar:
--
-- λ 2 #> P [0,1,2,3]
--   3     2
-- 6x  + 4x  + 2x
-- <BLANKLINE>

-- | Polynomials may be computed:
-- λ P [0,1,2,3] #@ 10
-- 3210

computeAt :: (VectorSpace a b, Num b) => Polynomial a -> b -> b
computeAt (Polynomial coefs) x = sum $ zipWith computeOne coefs [0..]
  where
    computeOne coef power = coef #> (x ^ power)

(#@) :: (VectorSpace a b, Num b) => Polynomial a -> b -> b
(#@) = computeAt

derivative :: (Num a, Enum a) => Polynomial a -> Polynomial a
derivative (Polynomial (_:cs)) = Polynomial $ zipWith (Prelude.*) cs [1..]
derivative (Polynomial [ ]) = Polynomial [ ]

binomial n k | k > n = 0 | otherwise = product [1..n] `div` (product [1..k] * product [1..n-k])
-- ^
-- α \x -> x >= 0 ==> (sum $ binomial x <$> [0..x]) == 2 ^ x

binomialPolynomial p = Polynomial $ binomial p <$> [0..p]

integerDerivative :: (Num a, Enum a, Integral a) => Polynomial a -> Polynomial a
integerDerivative p@(Polynomial xs) = Polynomial . reverse . unP $ substitutedSum #+ invert p
  where
    substitutedSum = foldl' (#+) (Polynomial [ ]) $ zipWith integerDerivativeOne xs [0..]
    integerDerivativeOne x p = x #> binomialPolynomial p

differential :: (Num a, Enum a) => Polynomial a -> Polynomial a
differential p = p #+ (invert . derivative $ p)
