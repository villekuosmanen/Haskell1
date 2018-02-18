module BST where

import Data.List

type Name = String

data Tree = Empty | Node (Name, (Either Float Int)) Tree Tree deriving Show

treeContains :: Name -> Tree -> Bool
treeContains name tree = case tree of
  Empty -> False -- empty tree
  Node (nName, nVal) left right | nName == name -> True
  Node (nName, nVal) left right ->
    if name `compareTo` nName < 0
      then (treeContains name left)
      else (treeContains name right)

treeInsert :: (Name, (Either Float Int)) -> Tree -> Tree
treeInsert (name, val) tree = case tree of
  Empty -> Node (name, val) Empty Empty -- empty tree is replaced with a node containing value
  Node (nName, nVal) left right ->
    if name `compareTo` nName < 0
      then Node (nName, nVal) (treeInsert (name, val) left) right
      else Node (nName, nVal) left (treeInsert (name, val) right)

treeUpdate :: (Name, (Either Float Int)) -> Tree -> Tree
treeUpdate (name, val) tree = case tree of
  Node (nName, nVal) left right ->
    if name == nName
      then do let oldNode = Node (nName, nVal) left right
              treeInsert (name, val) (deleteNode oldNode)
      else if name `compareTo` nName < 0
        then treeUpdate (name, val) left
        else if name `compareTo` name > 0
          then treeUpdate (name, val) right
          else treeInsert (name, val) tree

deleteTree :: Tree -> Name -> Tree
deleteTree Empty _ = Empty
deleteTree (Node (nName, nVal) left right) name
  | name == nName              = deleteNode (Node (nName, nVal) left right)
  | name `compareTo` nName < 0 = Node (nName, nVal) (deleteTree left name) right
  | name `compareTo` nName > 0 = Node (nName, nVal) left (deleteTree right name)

deleteNode :: Tree -> Tree
deleteNode (Node (name, val) Empty right) = right
deleteNode (Node (name, val) left Empty) = left
deleteNode (Node (name, val) left right) = (Node (nName, nVal) left right)
  where (nName, nVal) = findLastLeftElem right

findLastLeftElem :: Tree -> (Name, (Either Float Int))
findLastLeftElem (Node (name, val) Empty _) = (name, val)
findLastLeftElem (Node _ left _) = findLastLeftElem left

getNode :: Name -> Tree -> Either String (Either Float Int)
getNode name tree = case tree of
  Empty -> Left ("Error: Variable " ++ name ++ " not in scope")
  Node (nName, nVal) left right ->
    if treeContains name tree then do
      if name == nName then Right nVal
        else if name `compareTo` nName < 0
          then getNode name left
          else getNode name right
    else Left ("Error: Variable " ++ name ++ " not in scope")

-- if XS > YS, return 1, if XS < YS, return -1, if equal, return 0
compareTo               :: [Char] -> [Char] -> Int
compareTo [] (y:ys)     = 0
compareTo (x:xs) []     = 0
compareTo [] []         = 0
compareTo (x:xs) (y:ys) | x < y            = -1
                        | x > y            = 1
                        | otherwise        = compareTo xs ys
