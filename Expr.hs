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
          | Modulo Expr Expr
          | Power Expr Expr
          | Abs Expr
          | Val Int
          | ValueOf Name  --Evaluating variables - only supports char, not string
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
             | AccessCmdHistory Int
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
eval vars (Modulo x y) = Just (mod) <*> eval vars x <*> eval vars y
eval vars (Abs x) = Just abs <*> eval vars x
eval vars (Power x y) = Just (^) <*> eval vars x <*> eval vars y

digitToInt :: [Char] -> Int
digitToInt ds = read ds

pCommand :: Parser Command
pCommand = do t <- ident
              char '='
              e <- pExpr
              return (Set t e)
            ||| do e <- pExpr
                   return (Eval e)
                   ||| do char ':'
                          char 'q'
                          return Quit
                          ||| do char '!'
                                 n <- nat
                                 return (AccessCmdHistory n)

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
pFactor = do ds <- many1 digit
             return (Val (digitToInt ds))
           ||| do vs <- ident
                  return (ValueOf vs)
                ||| do char '|'
                       e <- pExpr
                       char '|'
                       return (Abs e)
                     ||| do char '('
                            e <- pExpr
                            char ')'
                            return e
   
pPower :: Parser Expr
pPower = do f <- pFactor
            do char '^'
               p <- pPower
               return (Power f p)
             ||| return f

pTerm :: Parser Expr
pTerm = do f <- pPower
           do char '*'
              t <- pTerm
              return (Multiply f t)
            ||| do char '/'
                   t <- pTerm
                   return (Divide f t)
                 ||| do char '%'
                        t <- pTerm
                        return (Modulo f t)
                      ||| return f
