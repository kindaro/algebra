{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  , TypeFamilies
  , RankNTypes
  , GADTs
  , DataKinds
  , KindSignatures
  , TypeOperators
  , StandaloneDeriving
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

module Algebra where

import Control.DeepSeq (NFData, rnf)
import Data.Function (fix)

-- $setup
-- λ import Control.Monad.Identity (runIdentity)
-- λ :set -XDataKinds

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

-- Binary type level fix:
-- ----------------------

-- |
-- λ g Nothing = 0; g (Just (x, y)) = x + y
-- λ cata2 g $ Fix2 (Just (1, Fix2 (Just (2, Fix2 Nothing))))
-- 3

newtype Fix2 f g = Fix2 { unFix2 :: g (f (Fix2 f g)) }

-- |
-- λ Fix2 (Just (1, Fix2 (Just (2, Fix2 Nothing))))
-- Fix2 {unFix2 = Just (1,Fix2 {unFix2 = Just (2,Fix2 {unFix2 = Nothing})})}

deriving instance Show (g (f (Fix2 f g))) => Show (Fix2 f g)

cata2 :: (Functor f, Functor g) => (g (f x) -> x) -> Fix2 f g -> x
cata2 alg = fix $ \f -> alg . (fmap.fmap) f . unFix2

cataM2 :: (Traversable f, Traversable g, Monad m) => (g (f x) -> m x) -> Fix2 f g -> m x
cataM2 alg = fix $ \f x -> alg =<< ((traverse.traverse) f . unFix2 $ x)

ana2 :: (Functor f, Functor g) => (x -> g (f x)) -> x -> Fix2 f g
ana2 coAlg = fix $ \f -> Fix2 . (fmap.fmap) f . coAlg

anaM2 :: (Traversable f, Traversable g, Monad m) => (x -> m (g (f x))) -> x -> m (Fix2 f g)
anaM2 coAlg = fix $ \f x -> fmap Fix2 . (traverse.traverse) f =<< coAlg x

-- Fix of arbitrary arity:
-- -----------------------

-- |
-- λ :t Fixes (FS (Just (FS ('a', FZ (Fixes (FS Nothing))))))
-- Fixes (FS (Just (FS ('a', FZ (Fixes (FS Nothing))))))
--   :: Fixes '[Maybe, (,) Char]
-- λ Fixes (FS (Just (FS ('a', FZ (Fixes (FS Nothing))))))
-- Fixes {unFixes = FS (Just (FS ('a',FZ (Fixes {unFixes = FS Nothing}))))}

newtype Fixes (xs :: [* -> *]) = Fixes {unFixes :: UnFixes xs}

deriving instance Show (UnFixes xs) => Show (Fixes xs)

type family UnFixes (xs :: [* -> *]) :: *
  where
    UnFixes '[ ] = Functors '[ ] (Fixes '[ ])
    UnFixes xs = Functors xs (Fixes xs)

data family Functors (xs :: [* -> *]) (i :: *) :: *
data instance Functors '[ ] i = FZ i
data instance Functors (x ': xs) i = FS (x (Functors xs i))

deriving instance Show i => Show (Functors '[ ] i)
deriving instance (Show (Functors xs i), Show (x (Functors xs i))) => Show (Functors (x ': xs) i)

instance Functor (Functors '[ ])
  where
    fmap f (FZ i) = FZ (f i)

instance (Functor (Functors xs), Functor x) => Functor (Functors (x ': xs))
  where
    fmap f (FS x) = FS (fmap (fmap f) x)

-- |
-- λ :{
--      let d' = Fixes (FS (Just (FS ('b', FZ (Fixes (FS Nothing))))))
--          d  = Fixes (FS (Just (FS ('a', FZ d'))))
--          f (FS Nothing) = [ ]
--          f (FS (Just (FS (x, FZ xs)))) = x: xs
--      in cataN f d
-- :}
-- "ab"

cataN :: Functor (Functors xs) => (Functors xs a -> a) -> Fixes xs -> a
cataN alg = fix $ \f -> alg . fmap f . unFixes

class Iso f g
  where
    to :: f -> g
    from :: g -> f

instance Iso (Functors (x ': xs) i) (x (Functors xs i))
  where
    to (FS x) = x
    from = FS

instance Iso (Functors '[ ] i) i
  where
    to (FZ x) = x
    from = FZ

instance (Iso a b, Functor f) => Iso (f a) (f b)
  where
    to = fmap to
    from = fmap from

-- |
-- λ x  = Just (1 :: Int)
-- λ x' = (FS (Just (FZ (1 :: Int))))
-- λ (to . (to :: Functors '[Maybe] Int -> Maybe (Functors '[ ] Int))) x' :: Maybe Int
-- Just 1
-- λ (from . (from :: Maybe Int -> Maybe (Functors '[ ] Int))) x :: Functors '[Maybe] Int
-- FS (Just (FZ 1))

-- instance Iso (Functors xs i) (Functors' xs i)  -- TODO.
