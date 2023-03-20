
-- module REPLCommand where

-- import Control.Applicative (many, (<|>))
-- import Lab2 (symbol, anychar)

-- data REPLCommand
--   = Quit
--   | Load String
--   | Eval String

-- replCommand :: Parser REPLCommand
-- replCommand = undefined

-- quit :: Parser REPLCommand
-- quit = do 
--     symbol ":q" <|> symbol ":quit"
--     return Quit

-- load :: Parser REPLCommand
-- load = do 
--     symbol ":l" <|> symbol ":load"
--     str <- many anychar
--     return $ Load str
    
-- eval :: Parser REPLCommand
-- eval = Eval <$> many anychar
-- ------- sau --- mai rau
-- eval :: Parser REPLCommand
-- eval = do
--     str <- many anychar
--     return $ Eval str

-- replCommand :: Parser REPLCommand
-- replCommand = undefined




-- -- implementare la lab dar exista in fisier------------------

-- main :: IO()
-- main = do
--     putStr "miniHaskell> "
--     s <- getLine
--     case parseFirst replCommand s of 
--         Nothing ->  do 
--             putStr "Cannot parse command" >> main
--         Just Quit -> return ()
--         Just(Load _) -> putStrLn "Not implemented" >> main
--         Just (Eval es) -> 
--             case parseFirst expreParser es of
--                 Nothing -> putStrLn "Error: cannot parse expression" >> main
--                 Just e -> putStrLn(showExp e) >> main
