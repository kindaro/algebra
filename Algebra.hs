{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  #-}

module Algebra where

import Data.List
import Data.Function (on)
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans (lift)

data Expr a = Branch [a] | Leaf !Int | Bud !String deriving (Eq, Show)

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

instance Eq (a (Fix a)) => Eq (Fix a) where
    (Fix x) == (Fix y) = x == y

branch = Fix . Branch
leaf   = Fix . Leaf
bud    = Fix . Bud

type Eval = RWST [(String, Int)] [(Expr (Fix Expr), Expr (Fix Expr))] () Maybe

evalSum :: Expr (Fix Expr) -> Eval (Fix Expr)
evalSum e@(Branch fxs) = me' >>= \e' ->
    if e == e'
        then return (Fix e)
        else tell [(e, e')] >> return (Fix e')

  where
    xs :: [Expr (Fix Expr)]
    xs = unFix <$> fxs

    me' :: Eval (Expr (Fix Expr))
    me' = (floatSingleton . fmap Fix . concat) <$> separated

    splitted :: Eval [[Expr (Fix Expr)]]
    splitted = do
        decorated <- sequence $ fmap (\x -> evaluable x >>= \flag -> return (x, flag)) xs
        return $ (fmap.fmap) fst $ groupBy ((==) `on` snd) $ decorated 

    separated :: Eval [[Expr (Fix Expr)]]
    separated = splitted >>= \splitted ->
        case splitted of
            [ ] -> return [ ]
            this@ ((x: _): ys) -> do
                flag <- evaluable x
                let evals = (if flag then [eval] else [ ]) ++ cycle [return, eval]
                sequence $ zipWith ($) evals this

                -- Due to the properties of `group`, `eval` should never be invoked on instances
                -- of Expr that are not evaluable. In such case `eval` will turn everything into
                -- Nothing.

evalSum x = return $ Fix x

eval :: [Expr a] -> Eval [Expr a]
eval = fmap (pure . Leaf) . eval'

eval' :: [Expr a] -> Eval Int
eval' [ ] = return 0
eval' (Leaf i: xs) = eval' xs >>= return . (+i)
eval' (Bud  s: xs) = asks (lookup s) >>= \maybei ->
    case maybei of
        Nothing -> lift Nothing
        Just i  -> eval' xs >>= lift . Just . (+i)

evaluable :: Expr a -> Eval Bool
evaluable x = case x of
    Branch xs -> return False
    Leaf   i  -> return True
    Bud    s  -> asks (lookup s) >>= \flag ->
        case flag of
           Just _  -> return True
           Nothing -> return False

floatSingleton :: [Fix Expr] -> Expr (Fix Expr)
floatSingleton [x] = unFix x
floatSingleton  xs = Branch xs


cata :: Functor f => (f b -> b) -> Fix f -> b
cata f = f . fmap (cata f) . unFix

type AlgebraM a f b = a b -> f b

cataM :: (Monad f, Traversable a) => AlgebraM a f b -> Fix a -> f b
cataM f x = f =<< (traverse (cataM f) . unFix $ x)

-- |
-- λ runRWS (cataM evalSum $ branch [branch [leaf 1, leaf 2], leaf 4, branch [bud "x", leaf 8]]) [("x", 32)] ()
-- (Leaf 47,(),[(Branch [Leaf 1,Leaf 2],Leaf 3),(Branch [Bud "x",Leaf 8],Leaf 40),(Branch [Leaf 3,Leaf 4,Leaf 40],Leaf 47)])
