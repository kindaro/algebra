{-# LANGUAGE
    PatternSynonyms
  , ViewPatterns
  , GeneralizedNewtypeDeriving
  #-}

module Filesystem where

import Algebra

import Data.Coerce
import Data.Function (fix)
import Control.Monad (void, join)

-- $setup
-- λ import Control.Monad.Identity (runIdentity)

data FS a = File String | Dir [IO a]
data FS' a = File' String | Dir' [a]
data Sys a = One (IO ()) | Some (IO [a])

instance Functor FS where
    fmap f (File x ) = File x
    fmap f (Dir mxs) = Dir ((fmap.fmap) f mxs)

-- instance Foldable FS where
--     foldMap = undefined
-- 
-- instance Traversable FS where
--     traverse f (File s) = pure $ File s
--     traverse f (Dir mxs) = fmap Dir $ do
--         xs <- mxs
--         let ys = traverse f xs
--         return ( _ ys)
--
-- It is apparent that a Traversable instance is impossible.
-- 
-- In general, it seems we can only have a Traversable instance of a chain of functors when each
-- of them is Traversable as well. No Traversable for IO means no Traversable for both [IO a] and
-- IO [a].

instance Functor FS' where
    fmap f (File' x ) = File' x
    fmap f (Dir' mxs) = Dir' (fmap f mxs)

instance Foldable FS' where
    foldMap = undefined

instance Traversable FS' where
    traverse _ (File' s) = pure $ File' s
    traverse f (Dir' xs) = fmap Dir' $ traverse f xs

pattern DirX :: [FS' (Fix FS')] -> FS' (Fix FS')
pattern DirX xs <- Dir' (coerce -> xs)
    where DirX = Dir' . fmap Fix

type AlgebraM' a f b = a b -> f b

-- cataM :: (Monad f, Traversable a) => AlgebraM a f b -> Fix a -> f b
-- cataM f x = f =<< (traverse (cataM f) . unFix $ x)

-- cataM' f (Fix x) = f =<< (traverse (cataM f) x)

execG :: FS String -> IO String
execG (File s) = return s
execG (Dir xs) = fmap mconcat . sequence $ xs

execG2 :: FS' String -> IO String
execG2 (File' s) = return s
execG2 (Dir' xs) = fmap unlines . sequence . fmap return $ xs

coalg1 ((== minBound) -> True) = Nothing
coalg1 x = Just (pred x)

ana alg i = Fix . fmap (ana alg) . alg $ i
-- ^
-- λ ana coalg1 (7::Word)
-- Just Just Just Just Just Just Just Nothing

fixana alg = fix $ \f -> Fix . fmap f . alg
-- ^
-- λ fixana coalg1 (7::Word)
-- Just Just Just Just Just Just Just Nothing
--
-- λ ana coalg1 (7::Word) == fixana coalg1 (7::Word)
-- True


type CoAlgebraM a f b = b -> a (f b)

finalCoAlgM :: (Monad m) => CoAlgebraM m a (Fix a)
finalCoAlgM = return . unFix

coalg1M :: (Enum b, Bounded b, Eq b, Monad a) => CoAlgebraM a Maybe b
coalg1M ((== minBound) -> True) = return Nothing
coalg1M x = return $ Just (pred x)

anaM :: (Traversable base, Monad m) => CoAlgebraM m base i -> i -> m (Fix base)
anaM alg i = fmap Fix . traverse (anaM alg) =<< (alg i)
-- ^
-- λ anaM coalg1M (7::Word)
-- Just Just Just Just Just Just Just Nothing
-- 
-- λ ana coalg1 (7::Word) == runIdentity (anaM coalg1M (7::Word))
-- True

fixanaM alg = fix $ \f -> \x -> fmap Fix . traverse f =<< alg x
-- ^
-- λ fixanaM coalg1M (7::Word)
-- Just Just Just Just Just Just Just Nothing
--
-- λ runIdentity (anaM coalg1M (7::Word)) == runIdentity (fixanaM coalg1M (7::Word))
-- True


-- exec :: FS () -> IO ()
-- exec (File s) = putStrLn s
-- exec (Dir mx) = void mx
