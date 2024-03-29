import Data.Char

newtype Parser' a' = Parser' { apply' :: String -> [(a', String)]}

newtype Parser a = Parser { apply :: String -> [(a, String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go [] = []   -- imposibil de parsat șirul vid
    go (c:input)
      | p c = [(c, input)]   -- dacă predicatul ține, întoarce c și restul șirului de intrare
      | otherwise = []       -- în caz contrar, imposibil de parsat

-- exemple
-- apply (satisfy isAlphaNum) "abc"
-- apply (satisfy isAlphaNum) "2bc"
-- apply (satisfy isAlphaNum) "@bc"

--- | Acceptă orice caracter
anychar :: Parser Char
anychar = satisfy (\_ -> True)
-- apply anychar "&ab"


--- | acceptă doar caracterul dat ca argument
char :: Char -> Parser Char
char x = satisfy (\c -> x == c )
-- apply (char 'i') "ionel"


--- | acceptă o cifră
digit :: Parser Char
digit = satisfy isDigit



--- | acceptă un spațiu (sau tab, sau sfârșit de linie -- vedeți funcția
-- din Data.Char )
space :: Parser Char
space = undefined


--- | succes doar dacă am șirul de intrare este vid
endOfInput :: Parser () 
endOfInput  = Parser go  
  where 
    go "" = [((), "")]    
    go _ = []

-- instance Functor Parser where fmap f pa = Parser (\input -> [(f a, rest) | (a, rest) <- apply pa input])
-- instance Applicative Parser where pure a = Parser (\input -> [(a, input)])    
-- pf <*> pa = Parser (\input -> [(f a, resta) | (f, restf) <- apply pf input, (a, resta) <- apply pa restf])

-- parseCifra = digitToInt <$> digit
-- digitToInt :: Char -> Int
--  apply (digitToInt <$> digit) "7ab"