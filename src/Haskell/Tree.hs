{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}

module Haskell.Tree where

import Text.Printf (PrintfType, printf)

rootTree = do
  -- print ""
  -- print $
  --   tree (
  --     Node 3
  --     (Node
  --       5 Nil Nil) Nil);

  -- print .
  --   show $
  --     listsInsert [7, 12, 6, 4, 8]

  print $ createTree [7, 12, 6, 4, 8, 2]
  -- print $ createList [1, 2, 3]


  -- print . foldl (\x l -> insert )  [1, 2, 3, 4]

{--
A tree is a collection of entities called nodes. Nodes are connected by edges. Each node contains a value or data, and it may or may not have a child node:
     Parent of 2 nodes   O --- Root Node
         Edges  -----   / \
                       O   O
                   Leafs  Leafs

  - Parent is a node that has an edge to a child node
  - Root is the topmost node of the tree
  - Child is a node that has a parent node
  - Leafs: Leaves are the last nodes on a tree. They are nodes {without children}. Like real trees, we have the root, branches, and finally the leaves.
  - Edges: handle relationship between nodes
  - Height is the length of the longest path to a leaf
  - Depth is the length of the path to its root

Dalam ilmu komputer, binary tree adalah struktur data pohon di mana setiap node memiliki paling banyak dua anak, yang disebut sebagai anak kiri dan anak kanan.

Binary trees are mainly used for searching and sorting as they provide a means to store data hierarchically

Binary tres:
  - data structure and algorithms
  - organized data
  - recursive data types

Binary Search Tree memiliki 3 oprasi dasar ,  hampir mirip dengan array keliatannya jadi ada 3 yaitu :

    Find(x)     : find value x didalam BST ( Search )
    Insert(x)   : memasukan value baru x ke BST ( Push )
    Remove(x)  : menghapus key x dari BST ( Delete )
https://gist.github.com/Kedrigern/1239141/5ee8f5f45facdf4f48785fc92a78ad4104f16537

BST Syarat:
- Nilai setiap node lebih besar dari nilai anak kirinya tetapi lebih kecil dari nilai anak kanannya.
-}
data Tree = Nil | Node Int Tree Tree

-- Nil :: Tree
-- Node :: Int -> Tree -> Tree -> Tree
tree :: Tree -> Int
tree Nil = 0
tree (Node _ l r) =
  1 + max (tree l) (tree r)

--
data TreeList a = Empty | Branch a (TreeList a) (TreeList a) deriving (Show)

-- instance (Show a) => Show (TreeList a) where
--   show t = "" ++ t

-- Sorting BST
insert :: (Ord a) => a -> TreeList a -> TreeList a
insert l Empty = Branch l Empty Empty
insert l (Branch root leftTree rightTree)
  | l == root = Branch root leftTree rightTree
  | l < root = Branch root (insert l leftTree) rightTree -- insert a node on a left
  | l > root = Branch root leftTree (insert l rightTree) -- insert a node on the right

-- listsInsert :: (Ord a) => [a] -> TreeList a
-- listsInsert [] = Empty
-- listsInsert a@(h : t) = insert h  t


-- anotherImpl :: (Foldable t, Ord a) => [a] -> t a 
-- anotherImpl [] = False
-- anotherImpl :: Foldable t => [a] -> t a -> a
-- anotherImpl [l] = foldl (\acc next -> acc == next) [] l

-- d = anotherImpl [1, 2]

-- go :: TreeList a
createTree :: [a] -> TreeList a
createTree [] = Empty
-- Using where
-- createTree xs = Branch a (createTree front) (createTree back) where 
--     n = length xs  
--     (front, a:back) = splitAt (n `div` 2) xs

-- splitAt = splitAt {index} {items} return tupple ([..], [...])

-- Using let
createTree xs = do
    let n = length xs in
      let (front, a:back) = splitAt (n `div` 2) xs in Branch a (createTree front) (createTree back)
-- Using where
createTree' :: [a] -> TreeList a
createTree' xs = Branch a (createTree' front) (createTree' back) where 
      n = length xs  
      (front, a:back) = splitAt (n `div` 2) xs

-- createList :: [a] -> [a]
-- createList [] = []
-- createList xs = a where 
--   (a:xs) = a

  -- n = length xs
  -- (front, x:back) = splitAt (n `div` 2) xs


-- Using case expr in lambda
mii = do
  foldl (\p a -> case p of [] -> p ++ [a]; p@(h:t) -> p ++ [a]) [] [1, 2, 3]

-- mii = insertion

{--
Arti:
  jika length [] === 0, return 0
  len(l) {
    if (l.length === 0) {
      return 0 
    }

    len()
  } 

--}

-- foldl (\x l -> x + 1) 0 [1,2,3] = 3 => + from left
-- foldr (\x l -> x + 1) 0 [1,2,3] = 2 => + from right

-- LEARN RECURSIVE
-- Check list Length 
lenRec :: Num a => p -> a
lenRec l = foldl (\acc _ -> acc + 1) 0 [1, 2, 3]

-- Sum all list, it's like reduce 
sumRec :: (Foldable t, Num a) => t a -> a
sumRec l = foldl (\acc next -> acc + next) 0 l

-- concat list
concatRec :: [a] -> [a] -> [a]
concatRec [] [] = []
concatRec l r = foldl (\acc next -> acc ++ [next]) l r


-- insert c@(h:t) = Branch (maximum c) Empty Empty

-- tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
--                                (Branch 'e' Empty Empty))
--                    (Branch 'c' Empty
--                                (Branch 'f' (Branch 'g' Empty Empty)
--                                            Empty))

{--
https://www.futurelearn.com/info/courses/functional-programming-haskell/0/steps/27215
https://www.cmi.ac.in/~spsuresh/teaching/prgh19/lectures/lecture22.pdf
https://www.educba.com/haskell-where/
https://www.freecodecamp.org/news/all-you-need-to-know-about-tree-data-structures-bceacb85490c/
https://wiki.haskell.org/99_questions/54A_to_60
https://www.cmi.ac.in/~madhavan/courses/prog2-2012/lectures/balanced-search-trees-in-haskell.txt
https://gist.github.com/Kedrigern/1239141/5ee8f5f45facdf4f48785fc92a78ad4104f16537
-}