{-# LANGUAGE
    PatternSynonyms
  , ViewPatterns
  , GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , DeriveAnyClass
  , StandaloneDeriving
  #-}

module Zip where  -- I will glue two arbitrary traversable structures.
                  -- But first I will need to understand Bartosz's
                  -- definiion of traversability.

import Prelude hiding (zip)
import Data.Monoid

import Algebra

data Chain i e = Link i e | End deriving (Functor, Show)

instance (Monoid i, Monoid e) => Monoid (Chain i e) where
    mempty = Link mempty mempty
    (Link i e) `mappend` (Link i' e') = Link (i <> i') (e <> e')
    x `mappend` End = x
    End `mappend` x = x


algebra :: Monoid i => Algebra (Chain i) i
algebra (Link i e) = i <> e
algebra End = mempty

coAlgebra1 :: CoAlgebra (Chain Int) Int
coAlgebra1 0 = End
coAlgebra1 x = Link x (pred x)

coAlgebra2 :: CoAlgebra (Chain Int) Int
coAlgebra2 7 = End
coAlgebra2 x = Link x (pred x)

zip :: Fix (Chain i) -> Fix (Chain i) -> Fix (Chain i)
zip = undefined
