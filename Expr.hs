module Expr where

import Parsing
import Control.Applicative
import Data.List
import Data.Tuple
import Data.Either

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
          | Val (Either Float Int)
          | ValueOf Name  --Evaluating variables - only supports char, not string
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
             | AccessCmdHistory Int
             | Quit
  deriving Show

eval :: [(Name, (Either Float Int))] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either String (Either Float Int) -- Result (int or float) or an error message 
eval vars (Val x) = Right x -- for values, just give the value directly
eval vars (ValueOf n) = find' n vars
    where find' n []         = Left ("Error: Variable " ++ n ++ " not in scope")
          find' n ((x,y):xs) = if x == n
                                  then Right y
                                  else find' n xs

eval vars (Add x y) = do let x' = eval vars x
                         let y' = eval vars y
                         add' x' y'
                         where
                            add' :: Either String (Either Float Int) -> Either String (Either Float Int) -> Either String (Either Float Int)
                            add' (Left xs) _                         = Left xs
                            add' _ (Left ys)                         = Left ys
                            add' (Right (Right x)) (Right (Right y)) = Right (Right (x + y))                                --integer addition
                            add' (Right x) (Right y)                 = Right (Left ((eitherToFloat x) + (eitherToFloat y))) --floaty addition
eval vars (Subtract x y) = do let x' = eval vars x
                              let y' = eval vars y
                              substract' x' y'
                              where
                                substract' :: Either String (Either Float Int) -> Either String (Either Float Int) -> Either String (Either Float Int)
                                substract' (Left xs) _                         = Left xs
                                substract' _ (Left ys)                         = Left ys
                                substract' (Right (Right x)) (Right (Right y)) = Right (Right (x - y))                                --integer substraction
                                substract' (Right x) (Right y)                 = Right (Left ((eitherToFloat x) - (eitherToFloat y))) --floaty substraction
eval vars (Multiply x y) = do let x' = eval vars x
                              let y' = eval vars y
                              multiply' x' y'
                              where
                                multiply' :: Either String (Either Float Int) -> Either String (Either Float Int) -> Either String (Either Float Int)
                                multiply' (Left xs) _                         = Left xs
                                multiply' _ (Left ys)                         = Left ys
                                multiply' (Right (Right x)) (Right (Right y)) = Right (Right (x * y))                                --integer multiplication
                                multiply' (Right x) (Right y)                 = Right (Left ((eitherToFloat x) * (eitherToFloat y))) --floaty multiplication
eval vars (Divide x y) = do let x' = eval vars x
                            let y' = eval vars y
                            divide' x' y'
                            where
                              divide' :: Either String (Either Float Int) -> Either String (Either Float Int) -> Either String (Either Float Int)
                              divide' (Left xs) _                         = Left xs
                              divide' _ (Left ys)                         = Left ys
                              divide' _ (Right (Right 0))                 = Left "Error: Division by zero"
                              divide' (Right (Right x)) (Right (Right y)) = Right (Right (x `div` y))                            --integer division
                              divide' (Right x) (Right y)                 = Right (Left ((eitherToFloat x) / (eitherToFloat y))) --floaty division
eval vars (Modulo x y) = do let x' = eval vars x
                            let y' = eval vars y
                            mod' x' y'
                            where
                              mod' :: Either String (Either Float Int) -> Either String (Either Float Int) -> Either String (Either Float Int)
                              mod' (Left xs) _                         = Left xs
                              mod' _ (Left ys)                         = Left ys
                              mod' (Right (Right x)) (Right (Right y)) = Right (Right (mod x y))                        --integer modulo
                              mod' _ _                                 = Left "Error: Modulo not defined for fractions" --Undefined
eval vars (Abs x) = do let x' = eval vars x
                       abs' x'
                       where
                         abs' :: Either String (Either Float Int) -> Either String (Either Float Int)
                         abs' (Left xs)         = Left xs
                         abs' (Right (Right x)) = Right (Right (abs x)) --abs for integers
                         abs' (Right (Left x))  = Right (Left (abs x))  --abs for floats
eval vars (Power x y) = Left ""--Right (^) <*> eval vars x <*> eval vars y

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
pFactor = do ds <- floatOrInt
             return (Val ds)
           ||| do char '-'
                  ds <- floatOrInt
                  return (Val (neg ds))
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

neg :: Either Float Int -> Either Float Int
neg (Left x) = Left (-x)
neg (Right x) = Right (-x)

eitherToFloat :: Either Float Int -> Float
eitherToFloat (Left x) = x
eitherToFloat (Right x) = fromIntegral x