module Main where

import Parsing
import Expr
import REPL
import System.IO
import Control.Exception
import System.IO.Error

main :: IO ()
main = do putStrLn "Specify the path to input file: "
          filePath <- getLine
          handle <- try (openFile filePath ReadMode)
          case handle of
              Left e ->
                if isDoesNotExistError e
                  then do putStrLn("Wrong input file path!")
                          main
                  else ioError e
              Right handle ->
                do repl initState handle
                   hClose handle
