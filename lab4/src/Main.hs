
module Main where

import System.IO

-- import C:\Univeristetea\FLP\lab4\Parsing.hs
-- ghc --make Lab2.hs
import Exp
 
import REPLCommand ( REPLCommand(Eval, Quit, Load), replCommand )
import Parsing ( parseFirst, exprParser )
import Printing (showExp)


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



