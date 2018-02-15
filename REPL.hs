module REPL where

import Expr
import Parsing
import Data.Maybe
import System.IO
import System.IO.Error
import System.Exit
import Control.Exception

data CustomType = CustomType { file :: Handle }

data State = State { vars :: [(Name, Int)],
                     numCalcs :: Int,
                     history :: [Command] }

initState :: State
initState = State [("it",0)] 0 []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars n x vars = dropVar n vars ++ [(n,x)]

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar n vars = filter (\(a,b) -> a /= n) vars

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = st {history = history st ++ [cmd]}

getCmd :: [Command] -> Int -> Command
getCmd cs n | length cs < n = error "Index too big"
            | otherwise     = cs!!(n-1)

processFiles :: State -> Command -> Handle -> IO ()
processFiles st (Set var e) handle
     = do let st' = addHistory st (Set var e)
          let resultMaybe = eval (vars st) e
          if var /= "it" && resultMaybe /= Nothing      -- protecting 'it' variable from modifications & check for errors
            then do putStrLn ("OK")
                    replFiles st' {vars = (updateVars var (fromJust resultMaybe ) (vars st))} handle
          else if var == "it"
                 then do putStrLn("Cannot modify the implicit 'it' variable.")
                         replFiles st' handle
               else do putStrLn("Error with evaluating expression") --Proper error handling should go here
                       replFiles st' handle
processFiles st (Eval e) handle
     = do let st' = addHistory st (Eval e)
          let resultMaybe = (eval (vars st') e)
          if resultMaybe == Nothing
              then do putStrLn ("Error with evaluating expression")  --Proper error handling should go here
                      replFiles st' handle
          else do putStrLn (show (fromJust resultMaybe))
                  replFiles st' {numCalcs = numCalcs st + 1, vars = updateVars "it" (fromJust resultMaybe) (vars st)} handle
processFiles st (AccessCmdHistory n) handle
     = do let newCmd = getCmd (reverse (history st)) n
          processFiles st newCmd handle
processFiles st Quit handle
     = putStrLn("Bye")

processUserInput :: State -> Command -> IO ()
processUserInput st (Set var e)
     = do let st' = addHistory st (Set var e)
          let resultMaybe = eval (vars st) e
          if var /= "it" && resultMaybe /= Nothing      -- protecting 'it' variable from modifications & check for errors
              then do putStrLn ("OK")
                      repl st' {vars = (updateVars var (fromJust resultMaybe ) (vars st))}
          else if var == "it"
              then do putStrLn("Cannot modify the implicit 'it' variable.")
                      repl st'
               else do putStrLn("Error with evaluating expression") --Proper error handling should go here
                       repl st'
processUserInput st (Eval e)
     = do let st' = addHistory st (Eval e)
          let resultMaybe = (eval (vars st') e)
          if resultMaybe == Nothing
            then do putStrLn ("Error with evaluating expression")  --Proper error handling should go here
                    repl st'
          else do putStrLn (show (fromJust resultMaybe))
                  repl st' {numCalcs = numCalcs st + 1, vars = updateVars "it" (fromJust resultMaybe) (vars st)}
processUserInput st (AccessCmdHistory n)
     = do let newCmd = getCmd (reverse (history st)) n
          processUserInput st newCmd
processUserInput st Quit
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
                                        exitSuccess
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
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                          processUserInput st cmd
                  _ -> do putStrLn "Parse error"
                          repl st
