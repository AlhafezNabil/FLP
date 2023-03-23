
module REPLCommand where

import Lab2
import Control.Applicative (many, (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String

-- ":q" sau ":quit"
-- sau "l :str" sau ":load str"
-- sau "strExpression"

quit :: Parser REPLCommand
quit = do
  symbol ":q" <|> symbol ":quit"
  return Quit

load :: Parser REPLCommand
load = do
  symbol ":l" <|> symbol ":load"
  str <- many anychar
  return $ Load str

eval :: Parser REPLCommand
-- eval = Eval <$> many anychar
eval = do
  str <- many anychar
  return $ Eval str

replCommand :: Parser REPLCommand
replCommand = quit <|> load <|> eval

