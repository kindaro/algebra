{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  #-}

module Algebra where

import Control.DeepSeq (NFData, rnf)
import Data.Function (fix)

newtype Fix a = Fix { unFix :: a (Fix a) }

instance Show (a (Fix a)) => Show (Fix a) where
    show (Fix x) = show x

instance Eq (a (Fix a)) => Eq (Fix a) where
    (Fix x) == (Fix y) = x == y

instance NFData (a (Fix a)) => NFData (Fix a) where
    rnf (Fix x) = rnf x


cata :: Functor f => (f b -> b) -> Fix f -> b
cata f = f . fmap (cata f) . unFix

type AlgebraM a f b = a b -> f b

cataM :: (Monad f, Traversable a) => AlgebraM a f b -> Fix a -> f b
cataM f x = f =<< (traverse (cataM f) . unFix $ x)

ana alg = Fix . fmap (ana alg) . alg

fixana alg = fix $ \f -> Fix . fmap f . alg

type CoAlgebraM a f b = b -> a (f b)

anaM :: (Traversable base, Monad m) => CoAlgebraM m base i -> i -> m (Fix base)
anaM alg i = fmap Fix . traverse (anaM alg) =<< (alg i)

fixanaM alg = fix $ \f -> \x -> fmap Fix . traverse f =<< alg x
