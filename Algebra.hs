{-# LANGUAGE
    FlexibleInstances
  , UndecidableInstances
  , PatternSynonyms
  , ViewPatterns
  , GeneralizedNewtypeDeriving
  #-}

module Algebra where

import Data.List
import Control.DeepSeq (NFData, rnf)
import Control.Monad.Trans.RWS.Strict
import Data.Coerce

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
