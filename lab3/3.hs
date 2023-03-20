-- pa >>= k = Parser (\input -> [(b, restb) | 
--     (a, resta) <- apply pa input,
--     (b, restb) <- apply (k a) resta])

-- instance Monad Parser where
-- pa >>= k = Parser (\input -> [(b, restb) | 
--     (a, resta) <- apply pa input,
--     (b, restb) <- apply (k a) resta])
-- instance Monad Parser where
-- pa >>= k = Parser (\input -> [(b, restb) | 
--     (a, resta) <- apply pa input,
--     (b, restb) <- apply (k a) resta])


-- cifsemn :: Parser cifSemn = do
--     ch <- satisfy (\x -> elem x "+-")
--     d <- digitToInt <$> digit
--     case ch of 
--         '+' -> return  d
--         '-' -> return (-d)

-- newType Parser a = Parser { apply :: string -> [(a,string)]}

-- instance Applicative Parser where
--  pure a = Parser (\input -> [(a, input)])
--  pf <*> pa = Parser (\input -> [(f a, resta) | (f, restf) <- apply pf input, (a, resta) <- apply pa restf])
