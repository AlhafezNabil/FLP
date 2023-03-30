
module REPLCommand where

import Control.Applicative (many, (<|>))
newtype IndexedVar = IndexedVar { ivName :: String, ivCount :: Int}

data Exp = x IndexedVar
  | App Exp Exp
  | Lam IndexedVar Exp


  data REPLCommand
  = Quit
  | Load String
  | Eval String