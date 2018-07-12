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

naiveFix :: (a -> a) -> a
naiveFix f = f (naiveFix f)

instance Show (a (Fix a)) => Show (Fix a) where
    show (Fix x) = show x

instance Eq (a (Fix a)) => Eq (Fix a) where
    (Fix x) == (Fix y) = x == y

instance NFData (a (Fix a)) => NFData (Fix a) where
    rnf (Fix x) = rnf x

type Algebra f a = f a -> a

type AlgebraM f m a = f a -> m a

type CoAlgebra f a = a -> f a

type CoAlgebraM f m a = a -> m (f a)

type Fmap f a = (Fix f -> a) -> f (Fix f) -> f a

cata :: Functor f => Algebra f b -> Fix f -> b
cata alg = alg . fmap (cata alg) . unFix

fixcata :: Functor f => Algebra f b -> Fix f -> b
fixcata alg = fix $ \f -> alg . fmap f . unFix

cata' :: Fmap f a -> Algebra f a -> Fix f -> a
cata' fmap alg = fix $ \f -> alg . fmap f . unFix

cataM :: (Monad m, Traversable f) => AlgebraM f m a -> Fix f -> m a
cataM alg x = alg =<< (traverse (cataM alg) . unFix $ x)

fixcataM :: (Monad m, Traversable f) => AlgebraM f m a -> Fix f -> m a
fixcataM alg = fix $ \f x -> alg =<< (traverse f . unFix $ x)

ana :: Functor f => CoAlgebra f a -> a -> Fix f
ana coAlg = Fix . fmap (ana coAlg) . coAlg

fixana :: Functor f => CoAlgebra f a -> a -> Fix f
fixana coAlg = fix $ \f -> Fix . fmap f . coAlg

anaM :: (Traversable f, Monad m) => CoAlgebraM f m a -> a -> m (Fix f)
anaM coAlg x = fmap Fix . traverse (anaM coAlg) =<< coAlg x

fixanaM :: (Traversable f, Monad m) => CoAlgebraM f m a -> a -> m (Fix f)
fixanaM coAlg = fix $ \f x -> fmap Fix . traverse f =<< coAlg x

-----

naiveFixcata :: Functor f => Algebra f b -> Fix f -> b
naiveFixcata alg = naiveFix $ \f -> alg . fmap f . unFix

naiveFixcataM :: (Monad m, Traversable f) => AlgebraM f m a -> Fix f -> m a
naiveFixcataM alg = naiveFix $ \f x -> alg =<< (traverse f . unFix $ x)

naiveFixana :: Functor f => CoAlgebra f a -> a -> Fix f
naiveFixana coAlg = naiveFix $ \f -> Fix . fmap f . coAlg

naiveFixanaM :: (Traversable f, Monad m) => CoAlgebraM f m a -> a -> m (Fix f)
naiveFixanaM coAlg = naiveFix $ \f x -> fmap Fix . traverse f =<< coAlg x

-- Functor composition:
-- --------------------

-- |
-- λ f x = if x > 0 then Compose (Just (x, pred x)) else Compose Nothing
-- λ ana f 2
-- Compose {unCompose = Just (2,Compose {unCompose = Just (1,Compose {unCompose = Nothing})})}

newtype Compose f g x = Compose { unCompose :: g (f x) } deriving Show

instance (Functor f, Functor g) => Functor (Compose f g)
  where
    fmap f = Compose . (fmap . fmap) f . unCompose
