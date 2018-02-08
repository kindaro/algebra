{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  , PatternSynonyms
  , ViewPatterns
  , GeneralizedNewtypeDeriving
  #-}

module Algebra where

import Data.List
import Control.Monad.Trans.RWS.Strict
import Data.Coerce

data Expr a = Branch [a] | Leaf !Int | Bud !String deriving (Eq, Show)

instance Functor Expr where
    fmap f (Branch xs) = Branch (fmap f xs)
    fmap _ (Leaf   i ) = Leaf    i
    fmap _ (Bud    s ) = Bud     s

instance Foldable Expr where
    foldMap = undefined

instance Traversable Expr where
    traverse f (Branch xs) = fmap Branch $ traverse f xs
    traverse _ (Leaf   i ) = pure $ Leaf i
    traverse _ (Bud    s ) = pure $ Bud  s

newtype Fix a = Fix { unFix :: a (Fix a) }

instance Show (a (Fix a)) => Show (Fix a) where
    show (Fix x) = show x

instance Eq (a (Fix a)) => Eq (Fix a) where
    (Fix x) == (Fix y) = x == y

type F = Fix Expr

type EF = Expr F

pattern Branch' :: [EF] -> EF
pattern Branch' xs <- Branch (coerce -> xs)
    where Branch' = Branch . fmap Fix

-- |
-- λ :{
--      let f (Branch' x) = x
--      in  f $ Branch' [Leaf 1, Bud "x"]
-- :}
-- [Leaf 1,Bud "x"]
--
-- λ :{
--      let f (Branch' x) = x
--      in  f $ Leaf 1
-- :}
-- *** Exception: <interactive>:...: Non-exhaustive patterns in function f
-- <BLANKLINE>

type Eval = RWST [(String, Int)] (Log (Message EF)) () Maybe

data Message a = Message a a deriving Eq

instance Show a => Show (Message a) where
    show (Message e e') = show e ++ " => " ++ show e'

newtype Log a = Log { unLog :: [a] } deriving Monoid

instance Show a => Show (Log a) where
    show = unlines . fmap show . unLog

compareAndTell :: (Monad m, Eq a) => a -> a -> RWST r (Log (Message a)) s m a
compareAndTell e e' | e == e'   =                   return e
                    | otherwise = tell (Log [Message e e']) >> return e'

evalSum :: EF -> Eval F
evalSum e@(Branch' xs) = Fix <$> compareAndTell e (deforest . fuseLeaves $ xs)
evalSum e@(Bud _)      = Fix <$> evalBud e
evalSum x              = Fix <$> return x

evalBud :: EF -> Eval EF
evalBud e@(Bud  s) = (asks . lookup $ s) >>= \x ->
    compareAndTell e $ case x of
        Nothing -> Bud s
        Just i  -> Leaf i
evalBud x = return x

fuseLeaves :: [EF] -> [EF]
fuseLeaves = foldl' (+:) [ ]
  where
    [ ]    +: x = [x]
    (y:ys) +: x = case (x, y) of
        (Leaf u, Leaf v) -> Leaf (u + v) : ys
        (_, _) -> x: y: ys

deforest :: [EF] -> EF
deforest [ ] = Branch' [ ]
deforest [x] = x
deforest  xs = Branch' xs


cata :: Functor f => (f b -> b) -> Fix f -> b
cata f = f . fmap (cata f) . unFix

type AlgebraM a f b = a b -> f b

cataM :: (Monad f, Traversable a) => AlgebraM a f b -> Fix a -> f b
cataM f x = f =<< (traverse (cataM f) . unFix $ x)

-- |
-- λ :{
--      let e = Branch' [Branch' [Leaf 1, Leaf 2], Leaf 4, Branch' [Bud "x", Leaf 8]]
--          (Just (res, log)) = evalRWST (cataM evalSum . Fix $ e) [("x", 32)] ()
--      in res
-- :}
-- Leaf 47
-- λ :{
--      let e = Branch' [Branch' [Leaf 1, Leaf 2], Leaf 4, Branch' [Bud "x", Leaf 8]]
--          (Just (res, log)) = evalRWST (cataM evalSum . Fix $ e) [("x", 32)] ()
--      in log
-- :}
-- Branch [Leaf 1,Leaf 2] => Leaf 3
-- Bud "x" => Leaf 32
-- Branch [Leaf 32,Leaf 8] => Leaf 40
-- Branch [Leaf 3,Leaf 4,Leaf 40] => Leaf 47
-- <BLANKLINE>
