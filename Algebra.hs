module Algebra where

data Expr a = Branch [a] | Leaf Int

instance Functor Expr where
    fmap f (Branch xs) = Branch (fmap f xs)
    fmap _ (Leaf   i ) = Leaf    i

newtype Fix a = Fix { unFix :: a (Fix a) }

branch = Fix . Branch
leaf   = Fix . Leaf

evalSum (Branch xs) = sum xs
evalSum (Leaf   i ) =     i

cata f = f . fmap (cata f) . unFix

λ cata evalSum $ branch [branch [leaf 1, leaf 2], leaf 3]
6
