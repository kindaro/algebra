{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  , PatternSynonyms
  , ViewPatterns
  #-}

module Algebra where

import Data.List
import Control.Monad.Trans.RWS.Strict

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

unbranch :: EF -> Maybe [EF]
unbranch (Branch fxs) = Just $ unFix <$> fxs
unbranch _ = Nothing

pattern Branch' :: [EF] -> EF
pattern Branch' xs <- (unbranch -> Just xs)
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

type Eval = RWST [(String, Int)] [(EF, EF)] () Maybe

compareAndTell :: (Monad m, Eq a) => a -> a -> RWST r [(a, a)] s m a
compareAndTell e e' | e == e'   =                   return e
                    | otherwise = tell [(e, e')] >> return e'

evalSum :: EF -> Eval F
evalSum e@(Branch' xs) = me' >>= \e' -> fmap Fix $ compareAndTell e e'
  where
    me' :: Eval EF
    me' = do 
        xs' <- sequence . fmap evalTotal $ xs
        return . deforest . fuseLeaves $ xs'
evalSum x = return $ Fix x

evalTotal :: Expr a -> Eval (Expr a)
evalTotal (Bud  s) = (asks . lookup $ s) >>= \x ->
    return $ case x of
        Nothing -> Bud s
        Just i  -> Leaf i
evalTotal x = return x

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
-- λ runRWST (cataM evalSum . Fix $ Branch' [Branch' [Leaf 1, Leaf 2], Leaf 4, Branch' [Bud "x", Leaf 8]]) [("x", 32)] ()
-- Just (Leaf 47,(),[(Branch [Leaf 1,Leaf 2],Leaf 3),(Branch [Bud "x",Leaf 8],Leaf 40),(Branch [Leaf 3,Leaf 4,Leaf 40],Leaf 47)])
