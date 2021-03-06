{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  #-}

module Cartesian where

import Prelude hiding ()
import Data.Array

-- | I believe we will eventually have an automagic Traversable => Gluable... but maybe not. In
--   the meantime, this will serve.
class Gluable f where
    glue :: (a -> b -> c) -> f a -> f b -> f c

-- | This instance is the obvious replica of zip.
instance Gluable [] where
    glue = zipWith

type A e = Array Int e

-- | Invariant: length _values = product _dimensions.
--   Another, more complicated invariant: for any dimension d, the projection of the initial
--   cartesian to d is equal for points with indices equal modulo length d.
data Cartesian v = Cartesian  -- In the initial cartesian, `v` would be a tuple.
    { _dimensions :: [Int]  -- ^ The head of the list is the header of the
                                          --   hyperinterval.
    , _values :: A v
    } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Gluable Cartesian where
    glue f x y | _dimensions x == _dimensions y
                    = Cartesian
                        { _dimensions = _dimensions x
                        , _values = arr $ zipWith f (unarr $ _values x) (unarr $ _values y)
                        }
               | otherwise = error $ "So far, gluable instance for \
                    \incomplete spaces is not defined."

-- ^ An example of gluing and an fmap:
--
-- λ :{
--     let m  = (appendWith (,) (uni [1..3]) (uni ['a'..'c']))
--         m' = (appendWith (,) (uni [7..9]) (uni ['E'..'G']))
--     unarr . _values $ fmap (\(x, y) -> [snd x, snd y]) $ glue (,) m m'
--   :}
-- ["aE","bF","cG","aE","bF","cG","aE","bF","cG"]


-- | This is only a monoidal empty if we use a smart constructor that detects it and applies the
--   necessary isomorphism. Such a smart constructor would only be useable with the initial
--   Cartesian, which we cannot define without advanced type level trickery.
empty :: Cartesian v
empty = Cartesian [1] (arr [undefined])

-- | Convert n-dimensional Cartesian to n inlayed lists.
--   This is actually doable with a Fix, but I will leave unimplemented for now.
-- toLists

-- | Get elements of an array such that they all belong to a congruence class c modulo n.
getByCongruentIndices :: Int -> Int -> A v -> [v]
getByCongruentIndices n c arr = let (low, high) = bounds arr
                                in  (arr !) <$> [low + c, low + c + n.. high]

autoListArray :: [a] -> A a
autoListArray xs = listArray (0, pred (length xs)) xs

arr :: [v] -> A v
arr = autoListArray

unarr :: A v -> [v]
unarr = elems

congr :: Int -> Int -> A v -> [v]
congr = getByCongruentIndices

congr0 :: Int -> A v -> [v]
congr0 n = congr n 0

-- | The size of a Cartesian.
card :: Cartesian a -> Int
card Cartesian{..} = product _dimensions

-- | Consruct a uni-dimensional Cartesian.
--
-- λ unarr . _values $ uni [3,2,1]
-- [3,2,1]
uni :: [v] -> Cartesian v
uni vs = Cartesian { _dimensions = [length vs], _values = arr vs }

-- | Dimension increment.
--
-- λ unarr . _values $ cons (,) ['a'..'c'] (uni [1,2,3])
-- [('a',1),('a',2),('a',3),('b',1),('b',2),('b',3),('c',1),('c',2),('c',3)]
cons :: (u -> v -> w) -> [u] -> Cartesian v -> Cartesian w
cons f xs Cartesian{..} = Cartesian { _dimensions = length xs: _dimensions
                                    , _values = arr [ x `f` y | x <- xs, y <- unarr _values ] }

-- | Paramerised dimension increment.
inject :: (u -> v -> w) -> [u] -> Int -> Cartesian v -> Cartesian w
inject = undefined

-- | Dimensional decrement.
uncons :: (u -> (v, w)) -> Cartesian u -> Maybe ([v], Cartesian w)
uncons _ Cartesian { _dimensions = [] } = Nothing
uncons f Cartesian { _dimensions = (_: ds), _values = xs } = -- View patterns?
    let ys = fmap (fst . f) . congr0 (product ds) $ xs
        zs = fmap (snd . f) . take (product ds) . unarr $ xs
    in  Just (ys, Cartesian { _dimensions = ds, _values = arr zs })

-- | Parametrized dimensional decrement.
--   Example:
--   slice (\x -> fst x, snd x) 2 [ [1,2,3] [T, F] ] = [ (1, [T, F]), (2, [T, F]), (3 [T, F]) ]
slice :: (u -> [(v, w)]) -> Int -> Cartesian u -> ([v], Cartesian w)
slice = undefined

type BitField = Word

-- | This is the semigroupoidal operation on Cartesians.
--
-- λ appendWith (,) (uni [1..3]) (uni ['a'..'c']) == cons (,) [1..3] (uni ['a'..'c'])
-- True
--
-- λ unarr . _values $ appendWith (,) (uni [1..3]) (uni ['a'..'c'])
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]
appendWith :: (u -> v -> w) -> Cartesian u -> Cartesian v -> Cartesian w
appendWith f x y = Cartesian { _dimensions = _dimensions x ++ _dimensions y
                             , _values = arr [ x `f` y | x <- unarr (_values x), y <- unarr (_values y) ]
                             }

-- -- | This is the monoidal operation on Initial Cartesians.
-- (×) :: Cartesian u -> Cartesian v -> Cartesian (u + w)
-- x × y = undefined

-- | This is more than an inverse of (×). It can cut away any number of dimensions, not
--   necessarily adjacent.
-- cut :: (u -> v -> w) -> BitField -> Cartesian u -> Maybe (Cartesian v, Cartesian w)
-- cut f b x = undefined  -- I would like it to be more optimal than repeatedly bubbling and then
--                        -- cutting the top.

-- | Bubble: apply a cycle from 0 to (i - 1) to the dimensions. That is, make the i-th dimension 
--   the first. I believe bubbles to be the generators of the symmetric group.
--
--   There is the usual invariant for cycles here: bubble _ (n - 1), applied n times, is the
--   identity. This is also where we start to beat the Prelude: bubble 1 on a matrix is transpose.
(↑) :: Cartesian u -> Int -> Cartesian u
Cartesian{..} ↑ i =
    let d  = product . drop i $ _dimensions
        ds = take i _dimensions ++ drop (succ i) _dimensions  -- Delete the i-th.
    in  Cartesian
            { _dimensions = ds
            , _values = arr . concat $ ($ _values) <$> (congr d <$> [0..pred d])
            }
