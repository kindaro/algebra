{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  #-}

module Algebra where

import Control.DeepSeq (NFData, rnf)
import Data.Function (fix)

-- $setup
-- λ import Control.Monad.Identity (runIdentity)

alg :: Algebra Maybe Word
alg Nothing = 0
alg (Just x) = succ x
-- ^
-- λ ana coAlg (7 :: Word)
-- Just Just Just Just Just Just Just Nothing
--
-- λ cata alg . ana coAlg $ (7 :: Word)
-- 7
--
-- λ runIdentity (anaM (return . coAlg) (7 :: Word)) == ana coAlg (7 :: Word)
-- True
--
-- λ ana coAlg (7 :: Word) == fixana coAlg (7 :: Word)
-- True
--
-- λ (cata alg . ana coAlg) (7 :: Word) == (fixcata alg . ana coAlg) (7 :: Word) 
-- True

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

cata :: Functor f => Algebra f b -> Fix f -> b
cata alg = alg . fmap (cata alg) . unFix

fixcata :: Functor f => Algebra f b -> Fix f -> b
fixcata alg = fix $ \f -> alg . fmap f . unFix

type AlgebraM f m a = f a -> m a

cataM :: (Monad m, Traversable f) => AlgebraM f m a -> Fix f -> m a
cataM alg x = alg =<< (traverse (cataM alg) . unFix $ x)

type CoAlgebra f a = a -> f a

ana :: Functor f => CoAlgebra f a -> a -> Fix f
ana coAlg = Fix . fmap (ana coAlg) . coAlg

fixana :: Functor f => CoAlgebra f a -> a -> Fix f
fixana coAlg = fix $ \f -> Fix . fmap f . coAlg

type CoAlgebraM f m a = a -> m (f a)

anaM :: (Traversable f, Monad m) => CoAlgebraM f m a -> a -> m (Fix f)
anaM coAlg = fix $ \f x -> fmap Fix . traverse f =<< coAlg x
