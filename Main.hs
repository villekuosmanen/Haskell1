module Main where

import Parsing
import Expr
import REPL
import System.IO

main :: IO ()
main = do handle <- openFile "testRead.txt" ReadMode
          repl initState handle
          hClose handle
