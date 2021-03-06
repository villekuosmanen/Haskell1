module REPL where

import Expr
import Parsing
import BST
import Data.Maybe
import System.IO
import System.IO.Error
import System.Exit
import Control.Exception
import Data.Either

-- container for Handle used in reading input files
data CustomType = CustomType { file :: Handle }

-- calculator state
data State = State { vars :: Tree,
                     numCalcs :: Int,
                     history :: [Command] }

-- Initial calculator state with "it" variable
initState :: State
initState = State (Node ("it", (Right 0)) Empty Empty) 0 []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> (Either Float Int) -> Tree -> Tree
updateVars n x vars = put (n, x) vars

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = st {history = history st ++ [cmd]}

-- Get command from history of commands
getCmd :: [Command] -> Int -> Command
getCmd cs n | length cs < n = error "Index too big"
            | n < 1         = error "Negative index"
            | otherwise     = cs!!(n-1)

-- Process input from file
processFiles :: State -> Command -> Handle -> IO ()
processFiles st (Set var e) handle -- set variable
     = do let st' = addHistory st (Set var e)
          let result = eval (vars st) e
          if var == "it" -- protecting 'it' variable from modifications & check for errors
            then do putStrLn("Cannot modify the implicit 'it' variable.")
                    replFiles st' handle
            else process' st' result handle
              where
                process' st' (Left xs) handle = do putStrLn xs
                                                   replFiles st' handle
                process' st' (Right x) handle = do putStrLn ("OK")
                                                   replFiles st' {vars = (updateVars var x (vars st))} handle
processFiles st (Eval e) handle -- evaluate expression
     = do let st' = addHistory st (Eval e)
          let result = (eval (vars st') e)
          process' st' result handle
              where
                process' st' (Left xs) handle = do putStrLn xs
                                                   replFiles st' handle
                process' st' (Right (Left fl)) handle = do putStrLn (show fl)
                                                           replFiles st' {numCalcs = numCalcs st + 1, vars = updateVars "it" (Left fl) (vars st)} handle
                process' st' (Right (Right intg)) handle = do putStrLn (show intg)
                                                              replFiles st' {numCalcs = numCalcs st + 1, vars = updateVars "it" (Right intg) (vars st)} handle
processFiles st (AccessCmdHistory n) handle -- access command history
     = do if (n > length (history st))
            then do putStrLn("Error: Index too big")
                    replFiles st handle
            else
              if (n < 1)
                then do putStrLn("Error: Negative or 0 index")
                        replFiles st handle
                else do let newCmd = getCmd (reverse (history st)) n
                        processFiles st newCmd handle

processFiles st Quit handle -- quit
     = putStrLn("Bye")

-- Process manual user input
processUserInput :: State -> Command -> IO ()
processUserInput st (Set var e) -- set variable
     = do let st' = addHistory st (Set var e)
          let result = eval (vars st) e
          if var == "it" -- protecting 'it' variable from modifications & check for errors
            then do putStrLn("Cannot modify the implicit 'it' variable.")
                    repl st'
            else process' st' result
              where
                process' st' (Left xs) = do putStrLn xs
                                            repl st'
                process' st' (Right x) = do putStrLn ("OK")
                                            repl st' {vars = (updateVars var x (vars st))}
processUserInput st (Eval e) -- evaluate expression
     = do let st' = addHistory st (Eval e)
          let result = (eval (vars st') e)
          process' st' result
              where
                process' st' (Left xs) = do putStrLn xs
                                            repl st'
                process' st' (Right (Left fl)) = do putStrLn (show fl)
                                                    repl st' {numCalcs = numCalcs st + 1, vars = updateVars "it" (Left fl) (vars st)}
                process' st' (Right (Right intg)) = do putStrLn (show intg)
                                                       repl st' {numCalcs = numCalcs st + 1, vars = updateVars "it" (Right intg) (vars st)}
processUserInput st (AccessCmdHistory n) -- access command history
     = do if (n > length (history st))
            then do putStrLn("Error: Index too big")
                    repl st
            else
              if (n < 1)
                then do putStrLn("Error: Negative or 0 index")
                        repl st
                else do let newCmd = getCmd (reverse (history st)) n
                        processUserInput st newCmd
processUserInput st Quit -- quit
     = putStrLn("Bye")


-- Read, Eval, Print Loop for input files
-- This reads and parses the input using the pCommand parser, and calls
-- 'processFiles' to process the command.
-- 'processFiles' will call 'replFiles' when done, so the system loops.

replFiles :: State -> Handle -> IO ()
replFiles st handle = do putStr (show (numCalcs st) ++ " > ")
                         inp <- try (hGetLine handle)
                         case inp of
                           Left e ->
                             if isEOFError e
                                then do putStrLn("Encountered end of file. Exiting the program.")
                                        return ()
                                else ioError e
                           Right inp ->
                             do putStrLn (show inp)
                                case parse pCommand inp of
                                  [(cmd, "")] -> -- Must parse entire input
                                     processFiles st cmd handle
                                  _ -> do putStrLn "Parse error"
                                          replFiles st handle

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'processUserInput' to process the command.
-- 'processUserInput' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                          processUserInput st cmd
                  _ -> do putStrLn "Parse error"
                          repl st
