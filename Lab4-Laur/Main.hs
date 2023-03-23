
module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand

-- main :: IO ()
-- main = undefined


main :: IO()
main = do
    putStr "miniHaskell> "
    s <- getLine
    case parseFirst replCommand s of
        Nothing -> putStrLn "Cannot parse command" >> main
        Just Quit -> return ()
        Just (Load _ ) -> putStrLn "Not implemented" >> main
        Just (Eval es) ->
            case parseFirst exprParser es of
                Nothing -> putStrLn "Error: cannot parse expression" >> main
                Just e -> putStrLn (showExp e) >> main

