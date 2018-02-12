module Graph where

import Prelude hiding (lookup)

-- type Id = Int
-- 
-- data Node = Node
--     { _id :: Id  -- TODO: Unique.
--     , _pointers :: [Id]
--     , _value :: Integer
--     }
--     deriving Show
-- 
-- type Graph = [Node]
-- 
-- graph1 = [ Node 1 [2,3] 1, Node 2 [3] 2, Node 3 [] 4 ]
-- 
-- lookup :: Graph -> Id -> Maybe Node
-- lookup [ ]    _                 = Nothing
-- lookup (x:xs) id | _id x == id  = Just x
--                  | otherwise    = lookup id xs
-- 
-- fmap' :: Graph -> (Integer -> Integer) -> Node -> Node
-- fmap' graph f (Node id v xs) = do
--     deref <- sequence (lookup graph <$> x)
--     case deref of
--         Nothing -> error $ "Graph lookup failed for node " ++ show id ++ "."
--         Just ns -> fmap f ns
-- 
-- alg :: Graph -> Node -> Maybe Integer
-- alg graph (Node _ v xs) = do
--     dereferenced <- sequence (lookup graph <$> xs)
--     x
