
module Parsing where

import Exp

import RezLab2


import Control.Applicative (some, many, (<|>))
import Data.Char (isAlpha, isAlphaNum)
import Data.String (String)

parseFirst :: Parser a -> String -> Maybe a
parseFirst p s
  = case apply p s of
      [] -> Nothing
      (a,_):_ -> Just a

haskellId :: Parser String
haskellId = identifier (satisfy isAlpha)(satisfy isAlphaNum)

-- identifier se asigura ca primul caracter satisface isAlpha
-- iar restul caracterelor din sir satisface isAlphaNum
-- este definit in lab2.hs si a fost tema pentru azi

-- accept ca perator orice sir format din ~i@#%^&:?|>?+=-_
haskellOp :: Parser String
haskellOp = identifier isOp isOp
  where 
      isOp = satisfy (\x -> elem x "~i@#%^&:?|>?+=-_")

    



var :: Parser Var
var = do
  haskellIdVar <- haskellId
  return $ Var haskellIdVar
-- >>> parseFirst var "b is a var"
-- Just (Var {getVar = "b"})

varExp :: Parser ComplexExp
varExp = CX <$> var 
-- >>> parseFirst varExp "b is a var"
-- Just (CX (Var {getVar = "b"}))

lambdaExp :: Parser ComplexExp
lambdaExp = do
  symbol "\\"
  x <- var
  symbol "->"
  exp <- expr
  return $ CLam x exp
-- >>> parseFirst lambdaExp "\\x -> x"
-- Just (CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"})))

letExp :: Parser ComplexExp
letExp = undefined
-- >>> parseFirst letExp "let x := y in z"
-- Just (Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"})))

letrecExp :: Parser ComplexExp
letrecExp = undefined
-- >>> parseFirst letrecExp "letrec x := y in z"
-- Just (LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"})))

listExp :: Parser ComplexExp
listExp = undefined
-- >>> parseFirst listExp "[a,b,c]"
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})])

natExp :: Parser ComplexExp
natExp = undefined
-- >>> parseFirst natExp "223 a"
-- Just (Nat 223)

parenExp :: Parser ComplexExp
parenExp = undefined
-- >>> parseFirst parenExp "(a)"
-- Just (CX (Var {getVar = "a"}))

basicExp :: Parser ComplexExp
basicExp = undefined
-- >>> parseFirst basicExp "[a,b,c]"
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})])

expr :: Parser ComplexExp
expr = varExp
-- >>> parseFirst expr "\\x -> x y z t"
-- Just (CLam (Var {getVar = "x"}) (CApp (CApp (CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y"}))) (CX (Var {getVar = "z"}))) (CX (Var {getVar = "t"}))))

exprParser :: Parser ComplexExp
exprParser = whiteSpace *> expr <* endOfInput
-- >>> parseFirst exprParser "let x := 28 in \\y -> + x y"
-- Just (Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"})))))


-- continuat cu restul expresiilor de la lambdaExp pana la sfarsit
-- pentru list, folosit parser-ul din laboratorul 2
-- aveti brackets -> verfica sa aveti [, ] si un parser intre ele
-- parserul dintre paranteze trebuie sa fie un parser peste "," - commaSep,
-- deja definit