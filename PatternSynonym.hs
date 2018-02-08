{-# LANGUAGE
    PatternSynonyms
  , ViewPatterns
  #-}

module PatternSynonym where

import Data.Coerce


-- ### Type definitions.

-- | This wrapper may have been introduced to harden type safety, or define a
--   typeclass instance.  Its actual purpose does not matter for us here.

newtype Wrapped a = Wrap { unWrap :: a } deriving (Eq, Show)

-- | This data type exemplifies a non-trivial collection of things.
data MaybeThings a = Some [a] | None deriving (Eq, Show)

-- | This type synonym exemplifies a non-trivial collection of things that are
--   additionally wrapped.
type MaybeWrappedThings a = MaybeThings (Wrapped a)


-- ### An example of what we may want to do with our types:

-- | This is a function that does useful work on plain (not Wrapped!) Ints.
doSomething :: [Int] -> [Int]
doSomething = filter even
-- ^
--   λ doSomething [2, 3, 5]
--   [2]

-- | This is the example data we may have.
example :: MaybeWrappedThings Int
example = Some [Wrap 2, Wrap 3, Wrap 5]

-- | This is a function that must only accept a collection of wrapped things,
--   but it has to unwrap them in order to do something useful.
getThings :: MaybeWrappedThings Int -> [Int]
getThings (Some wxs) = doSomething xs
  where xs = unWrap <$> wxs
getThings None       = [ ]
-- ^
--   λ getThings example
--   [2]


-- ### An example of how we may simplify the same logic with the help of
--   pattern synonyms.

-- | This is the pattern synonym that allows us to somewhat simplify getThings.
pattern Some' :: [a] -> MaybeWrappedThings a
pattern Some' xs <- Some (coerce -> xs)
  where Some' = Some . coerce

-- | This is how we may define our data with the pattern synonym.
--   It's got linearly less lexemes!
example' :: MaybeWrappedThings Int
example' = Some' [2, 3, 5]
-- ^
--   λ example == example'
--   True

-- | This is how our function may look with the pattern synonym.
getThings' :: MaybeWrappedThings Int -> [Int]
getThings' (Some' xs) = doSomething xs
getThings' None       = [ ]
-- ^
--   λ getThings' example'
--   [2]
--
--   λ getThings' example' == getThings example
--   True
