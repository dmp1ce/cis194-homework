-- | A library to do stuff.
module Lib
    (
      ourAdd
    , heightTree
    , insertTree
    , Tree (Node,Leaf)
    , foldTree
    , isTreeBalanced
    ) where

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node _ l d r)
    | heightTree left > heightTree right =
      Node (heightTree right + 1) l d right 
    | otherwise =
      Node (heightTree left + 1) left d r
  where
    left  = insertTree x l
    right = insertTree x r

heightTree :: Tree t -> Integer
heightTree (Node h _ _ _) = h
heightTree Leaf = 0

isTreeBalanced :: Tree t -> Bool
isTreeBalanced (Node _ l _ r) = abs (heightTree l - heightTree r) <= 1
isTreeBalanced Leaf = True
