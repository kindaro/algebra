{-# LANGUAGE
    TypeSynonymInstances
  #-}

module Algebra2 where

import Data.Bifunctor

data Expr a i = Branch [a] | Leaf i

instance Bifunctor Expr where
    bimap f _ (Branch xs) = Branch (fmap f xs)
    bimap _ g (Leaf   i ) = Leaf   (g i)

newtype Fix2 a i = Fix2 { unFix2 :: a (Fix2 a i) i }

branch = Fix2 . Branch
leaf   = Fix2 . Leaf

evalSum (Branch xs) = sum xs
evalSum (Leaf   i ) =     i

cata2 f g = f . bimap (cata2 f g) g . unFix2

-- |
-- λ cata2 evalSum (+1) $ branch [branch [leaf 1, leaf 2], leaf 3]
-- 9
