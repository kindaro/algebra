{-# LANGUAGE
    PatternSynonyms
  , ViewPatterns
  , GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  #-}

module Filesystem where

import Algebra

import System.FilePath
import System.Directory

-- $setup
-- λ import Control.Monad.Identity (runIdentity)

coalg1 :: (Enum a, Bounded a, Eq a) => a -> Maybe a
coalg1 ((== minBound) -> True) = Nothing
coalg1 x = Just (pred x)
-- ^
-- λ ana coalg1 (7::Word)
-- Just Just Just Just Just Just Just Nothing

finalCoAlgM :: (Monad m) => CoAlgebraM f m (Fix f)
finalCoAlgM = return . unFix

coalg1M :: (Enum a, Bounded a, Eq a, Monad m) => CoAlgebraM Maybe m a
coalg1M ((== minBound) -> True) = return Nothing
coalg1M x = return $ Just (pred x)
-- ^
-- λ anaM coalg1M (7::Word)
-- Just Just Just Just Just Just Just Nothing
-- 
-- λ ana coalg1 (7::Word) == runIdentity (anaM coalg1M (7::Word))
-- True

browseSimple :: FilePath -> IO (Either FilePath [FilePath])
browseSimple x = do
    isAvailable <- doesPathExist x
    if not isAvailable then error $ "File not found: " ++ x else do
        isFile <- doesFileExist x
        if isFile then return $ Left x else do
            isDirectory <- doesDirectoryExist x
            if not isDirectory then error $ "Unknown filesystem node: " ++ x else do
                listing <- listDirectory x
                return $ Right ((x </>) <$> listing)
-- ^
-- λ browseSimple  "."
-- Right [..."./Filesystem.hs"...]

newtype FS2 a = FS2 (Either String [a]) deriving (Functor, Foldable, Traversable)

instance (Show a) => Show (FS2 a) where
    show (FS2 (Left s)) = s
    show (FS2 (Right ss)) = init $ unlines $ show <$> ss

browse :: FilePath -> IO (FS2 String)
browse x = do
    isAvailable <- doesPathExist x
    if not isAvailable then error $ "File not found: " ++ x else do
        isFile <- doesFileExist x
        if isFile then return $ FS2 $ Left x else do
            isDirectory <- doesDirectoryExist x
            if not isDirectory then error $ "Unknown filesystem node: " ++ x else do
                listing <- listDirectory x
                return $ FS2 $ Right ((x </>) <$> listing)

-- exec :: FS () -> IO ()
-- exec (File s) = putStrLn s
-- exec (Dir mx) = void mx
