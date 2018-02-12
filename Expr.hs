module Expr where

import Parsing
import Control.Applicative
import Data.List
import Data.Tuple
import Data.Maybe

type Name = String

-- At first, 'Expr' contains only addition and values. You will need to
-- add other operations, and variables
data Expr = Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Val Int
          | ValueOf Name  --Evaluating variables - only supports char, not string
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
             | Quit
  deriving Show

eval :: [(Name, Int)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Int -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (ValueOf n) = find' n vars
    where find' _ []         = Nothing
          find' n ((x,y):xs) = if x == n then Just y else find' n xs

eval vars (Add x y) = Just (+) <*> eval vars y <*> eval vars x
eval vars (Subtract x y) = Just (-) <*> eval vars x <*> eval vars y
eval vars (Multiply x y) = Just (*) <*> eval vars x <*> eval vars y
eval vars (Divide x y) = Just (div) <*> eval vars x <*> eval vars y --currently returns ints

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = do t <- letter
              char '='
              e <- pExpr
              return (Set [t] e)
            ||| do e <- pExpr
                   return (Eval e)
                   ||| do char ':'
                          char 'q'
                          return Quit

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   return (Subtract t e)
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- digit
             return (Val (digitToInt d))
           ||| do v <- letter
                  return (ValueOf [v])
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Multiply f t)
            ||| do char '/'
                   t <- pTerm
                   return (Divide f t)
                 ||| return f
