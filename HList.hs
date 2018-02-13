{-# LANGUAGE
    DataKinds
  , KindSignatures
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , TypeFamilies
  , UndecidableInstances
  #-}

module HList where

import Unsafe.Coerce

data Nat = Zero | Succ Nat

type Nichtig = 'Zero
type Ein = 'Succ 'Zero
type Zwei = 'Succ Ein

newtype NummeriertInt (n :: Nat) = N Int

x :: NummeriertInt Nichtig
x = N 2

y :: NummeriertInt Ein
y = N 3

type family Sum n m where
    Sum 'Zero m = m
    Sum ('Succ n) m = Sum n ('Succ m)

data V (n :: Nat) a = V [a] deriving (Show, Functor, Foldable, Traversable)

v0 :: V Nichtig a
v0 = V []

ys :: V Zwei Int
ys = V [4, 5]

(%%) :: V l1 i -> V l2 i -> V (Sum l1 l2) i
(V xs) %% (V ys) = V (xs ++ ys)

(%:) :: i -> V l i -> V (Sum l Ein) i
x %: (V xs) = V (x:xs)


