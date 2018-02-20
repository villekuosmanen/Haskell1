{- Code adapted from
https://github.com/chinmay-ratnaparkhi/BST-Haskell/blob/master/myBST.hs
https://gist.github.com/camlorn/64e6c30ed6b6bf56f443
https://gist.github.com/Kedrigern/1239141/5ee8f5f45facdf4f48785fc92a78ad4104f16537
accessed on 20/02/2017
-}

module BST where

import Data.List

type Name = String

data Tree = Empty | Node (Name, (Either Float Int)) Tree Tree deriving Show

-- Checks if tree contains a value with the given name
treeContains :: Name -> Tree -> Bool
treeContains name tree = case tree of
   Empty -> False -- empty tree
   Node (nName, nVal) left right ->
     if name == nName then True
       else if name `compareTo` nName < 0
        then (treeContains name left)
        else (treeContains name right)

-- Checks if tree is empty
treeIsEmpty :: Tree -> Bool
treeIsEmpty Empty = True
treeIsEmpty _     = False

-- Puts a value in the tree
put :: (Name, (Either Float Int)) -> Tree -> Tree
put (name, val) tree = case tree of
  Empty -> Node (name, val) Empty Empty -- empty tree is replaced with a node containing value
  Node (nName, nVal) left right ->
    if name == nName
      then Node (name, val) left right
      else if name `compareTo` nName < 0
        then Node (nName, nVal) (put (name, val) left) right
        else Node (nName, nVal) left (put (name, val) right)

-- Deletes tree
deleteTree :: Tree -> Name -> Tree
deleteTree Empty _ = Empty
deleteTree (Node (nName, nVal) left right) name
  | name == nName              = deleteNode (Node (nName, nVal) left right)
  | name `compareTo` nName < 0 = Node (nName, nVal) (deleteTree left name) right
  | name `compareTo` nName > 0 = Node (nName, nVal) left (deleteTree right name)

-- Deletes a node
deleteNode :: Tree -> Tree
deleteNode (Node (name, val) Empty right) = right
deleteNode (Node (name, val) left Empty) = left
deleteNode (Node (name, val) left right) = (Node (nName, nVal) left right)
  where (nName, nVal) = findLastLeftElem right

-- Finds the last left element in the given tree
findLastLeftElem :: Tree -> (Name, (Either Float Int))
findLastLeftElem (Node (name, val) Empty _) = (name, val)
findLastLeftElem (Node _ left _) = findLastLeftElem left

-- Gets node containing the name provided
getNode :: Name -> Tree -> Either String (Either Float Int)
getNode name tree = case tree of
  Empty -> Left ("Error: Variable " ++ name ++ " not in scope")
  Node (nName, nVal) left right ->
      if name == nName
        then Right nVal
        else if name `compareTo` nName < 0
          then getNode name left
          else getNode name right

-- Compares names of variables:
-- if XS > YS, return 1, if XS < YS, return -1, if equal, return 0
compareTo               :: [Char] -> [Char] -> Int
compareTo [] (y:ys)     = 0
compareTo (x:xs) []     = 0
compareTo [] []         = 0
compareTo (x:xs) (y:ys) | x < y            = -1
                        | x > y            = 1
                        | otherwise        = compareTo xs ys
