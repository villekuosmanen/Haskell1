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

process :: State -> Command -> Handle -> IO ()
process st (Set var e) handle
     = do let st' = addHistory st (Set var e)
          if var /= "it" then do               -- protecting 'it' variable from modifications
            putStrLn ("OK")
            repl st' {vars = (updateVars var (fromJust (eval (vars st) e)) (vars st))} handle
          else do
            putStrLn("Cannot modify the implicit 'it' variable.")
            repl st' handle
process st (Eval e) handle
     = do let st' = addHistory st (Eval e)
          let it = fromJust (eval (vars st') e)
          putStrLn (show it)
          repl st' {numCalcs = numCalcs st + 1, vars = updateVars "it" it (vars st)} handle
process st (AccessCmdHistory n) handle
     = do let newCmd = getCmd (reverse (history st)) n
          process st newCmd handle
process st Quit handle
     = putStrLn("Bye")

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> Handle -> IO ()
repl st handle = do putStr (show (numCalcs st) ++ " > ")
                    inp <- try (hGetLine handle)
                    case inp of
                      Left e ->
                        if isEOFError e
                          then do putStrLn("Encountered end of file. Exiting the program.")
                                  exitFailure
                          else ioError e
                      Right inp ->
                        do putStrLn (show inp)
                           case parse pCommand inp of
                             [(cmd, "")] -> -- Must parse entire input
                               process st cmd handle
                             _ -> do putStrLn "Parse error"
                           repl st handle
