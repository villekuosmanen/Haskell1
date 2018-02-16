module Main where

import Parsing
import Expr
import REPL
import System.IO
import Control.Exception
import System.IO.Error

main :: IO ()
main = do putStr "Enter M for manual input, F for using input files: "
          userChoice <- getLine
          if userChoice == "M" then repl initState
          else
            if userChoice == "F" then do
              putStrLn "Specify the path to input file: "
              filePath <- getLine
              handle <- try (openFile filePath ReadMode)
              case handle of
                Left e ->
                  if isDoesNotExistError e
                    then do putStrLn("Wrong input file path!")
                            main
                    else ioError e
                Right handle ->
                  do replFiles initState handle
                     hClose handle
            else main
