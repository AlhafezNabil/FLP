
module REPLCommand where

import Control.Applicative (many, (<|>))
import RezLab2

data REPLCommand
  = Quit
  | Load String
  | Eval String

replCommand :: Parser REPLCommand
replCommand = undefined

