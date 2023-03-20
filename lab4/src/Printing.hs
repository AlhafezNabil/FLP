
module Printing (showExp) where

import Exp
import Data.List (intercalate)
import RezLab2

showVar :: Var -> String
showVar = getVar

showExp :: ComplexExp -> String
showExp (CX x) = showVar x
showExp (Nat n) = show n
showExp (CLam x e) = "(\\" ++ showVar x ++ " -> " ++ showExp e ++ ")"
showExp (CApp e1 e2) = "(\\" ++ showExp e1 ++ " -> " ++ showExp e2 ++ ")"
-- showExp (Let x e1 e2) = "(" ++ "let " ++





