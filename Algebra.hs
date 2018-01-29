module Algebra where

data Expr a = Branch [a] | Leaf Int

instance Functor Expr where
    fmap f (Branch xs) = Branch (fmap f xs)
    fmap _ (Leaf   i ) = Leaf    i

instance Foldable Expr where
    foldMap = undefined

instance Traversable Expr where
    traverse f (Branch xs) = fmap Branch $ traverse f xs
    traverse f (Leaf   i ) = pure $ Leaf i

newtype Fix a = Fix { unFix :: a (Fix a) }

branch = Fix . Branch
leaf   = Fix . Leaf

evalSum (Branch xs) = sum xs
evalSum (Leaf   i ) =     i

cata f = f . fmap (cata f) . unFix

-- λ cata evalSum $ branch [branch [leaf 1, leaf 2], leaf 3]
-- 6

type AlgebraM a f b = a b -> f b

cataM :: (Monad f, Traversable a) => AlgebraM a f b -> Fix a -> f b
cataM f x = f =<< (traverse (cataM f) . unFix $ x)

-- λ let printAndReturn x = print x >> pure x
-- λ cataM (printAndReturn . evalSum) $ branch [branch [leaf 1, leaf 2], leaf 3]
-- 1
-- 2
-- 3
-- 3
-- 6
-- 6
