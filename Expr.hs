{-# LANGUAGE
    PatternSynonyms
  , ViewPatterns
  , GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  #-}

module Expr where

import Data.List
import Control.Monad.Trans.RWS.Strict
import Data.Coerce

import Algebra

data Expr a = Branch [a] | Leaf !Int | Bud !String
    deriving (Eq, Show, Functor, Foldable, Traversable)

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
