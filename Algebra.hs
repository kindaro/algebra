{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  #-}

module Algebra where

import Data.List

data Expr a = Branch [a] | Leaf !Int | Bud !String deriving Show

instance Functor Expr where
    fmap f (Branch xs) = Branch (fmap f xs)
    fmap _ (Leaf   i ) = Leaf    i
    fmap _ (Bud    s ) = Bud     s

instance Foldable Expr where
    foldMap = undefined

instance Traversable Expr where
    traverse f (Branch xs) = fmap Branch $ traverse f xs
    traverse f (Leaf   i ) = pure $ Leaf i
    traverse f (Bud    s ) = pure $ Bud  s

newtype Fix a = Fix { unFix :: a (Fix a) }

instance Show (a (Fix a)) => Show (Fix a) where
    show (Fix x) = show x

branch = Fix . Branch
leaf   = Fix . Leaf
bud    = Fix . Bud

evalSum :: Expr (Fix Expr) -> Fix Expr
evalSum (Branch fxs) = floatSingleton . fmap Fix . concat $ separated
  where
    xs :: [Expr (Fix Expr)]
    xs = unFix <$> fxs

    splitted = groupBy (\x y -> evaluable x == evaluable y) xs 

    separated = case splitted of
        [ ] -> [ ]
        this@ ((x: _): ys) -> zipWith ($)
                                ((if evaluable x then [eval] else [ ]) ++ cycle [id, eval])
                                this

    eval :: [Expr a] -> [Expr a]
    eval = pure . Leaf . eval'

    eval' :: [Expr a] -> Int
    eval' [ ] = 0
    eval' (Leaf i: xs) = i + eval' xs
    eval'  x  = error $ "Algorithmic error: eval' can only evaluate leaves."

    evaluable :: Expr a -> Bool
    evaluable x = case x of
        Branch xs -> False
        Leaf   i  -> True
        Bud    s  -> False

evalSum x = Fix x

floatSingleton :: [Fix Expr] -> Fix Expr
floatSingleton [x] = x
floatSingleton  xs = Fix . Branch $ xs


cata f = f . fmap (cata f) . unFix

-- |
-- λ cata evalSum $ branch [branch [leaf 1, leaf 2], leaf 3]
-- Leaf 6
--
-- λ cata evalSum $ branch [branch [leaf 1, leaf 2], leaf 4, branch [bud "x", leaf 8]]
-- Branch [Leaf 7,Branch [Bud "x",Leaf 8]]

type AlgebraM a f b = a b -> f b

cataM :: (Monad f, Traversable a) => AlgebraM a f b -> Fix a -> f b
cataM f x = f =<< (traverse (cataM f) . unFix $ x)

-- |
-- λ let printAndReturn x = print x >> pure x
-- λ cataM (printAndReturn . evalSum) $ branch [branch [leaf 1, leaf 2], leaf 4, branch [bud "x", leaf 8]]
-- Leaf 1
-- Leaf 2
-- Leaf 3
-- Leaf 4
-- Bud "x"
-- Leaf 8
-- Branch [Bud "x",Leaf 8]
-- Branch [Leaf 7,Branch [Bud "x",Leaf 8]]
-- Branch [Leaf 7,Branch [Bud "x",Leaf 8]]
