{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  #-}

module Algebra where

import Control.DeepSeq (NFData, rnf)
import Data.Function (fix)

alg :: Algebra Maybe Word
alg Nothing = 0
alg (Just x) = succ x
-- ^
-- λ ana coAlg (7 :: Word)
-- Just Just Just Just Just Just Just Nothing
--
-- λ cata alg $ ana coAlg (7 :: Word)
-- 7

coAlg :: CoAlgebra Maybe Word
coAlg 0 = Nothing
coAlg x = Just (pred x)

newtype Fix a = Fix { unFix :: a (Fix a) }

instance Show (a (Fix a)) => Show (Fix a) where
    show (Fix x) = show x

instance Eq (a (Fix a)) => Eq (Fix a) where
    (Fix x) == (Fix y) = x == y

instance NFData (a (Fix a)) => NFData (Fix a) where
    rnf (Fix x) = rnf x

type Algebra f a = f a -> a

cata :: Functor f => (f b -> b) -> Fix f -> b
cata f = f . fmap (cata f) . unFix

type AlgebraM f m a = f a -> m a

cataM :: (Monad m, Traversable f) => AlgebraM f m a -> Fix f -> m a
cataM f x = f =<< (traverse (cataM f) . unFix $ x)

type CoAlgebra f a = a -> f a

ana :: Functor f => CoAlgebra f a -> a -> Fix f
ana coalg = fix $ \f -> Fix . fmap f . coalg

type CoAlgebraM f m a = a -> m (f a)

anaM :: (Traversable f, Monad m) => CoAlgebraM f m a -> a -> m (Fix f)
anaM coalg = fix $ \f x -> fmap Fix . traverse f =<< coalg x
