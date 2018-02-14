{-# LANGUAGE
    DataKinds
  , KindSignatures
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  , ViewPatterns
  , MultiWayIf
  #-}

module HList where

data Nat = Zero | Succ Nat

type Nichtig = Zero
type Ein = Succ Zero
type Zwei = Succ Ein

newtype NummeriertInt (n :: Nat) = N Int

x :: NummeriertInt Nichtig
x = N 2

y :: NummeriertInt Ein
y = N 3

type family n + m where
    Zero + m = m
    Succ n + m = Succ (n + m)

data V (n :: Nat) i = V [i] deriving (Show, Functor, Foldable, Traversable)

v0 :: V Nichtig a
v0 = V []

ys :: V Zwei Int
ys = V [4, 5]

(%%) :: V n i -> V m i -> V (n + m) i
(V xs) %% (V ys) = V (xs ++ ys)

(%:) :: i -> V n i -> V (n + Ein) i
x %: (V xs) = V (x:xs)


