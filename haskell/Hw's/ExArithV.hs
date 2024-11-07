module ExArithV where

import Data.Char qualified as C 

-- modify ExArith to allow for variables

-- decide how to represent Assignments:
-- type Assignment a = Var (Char, a)
data Var a = Var (Char, a) 
	deriving(Eq, Show)

assignment :: Char -> a -> Var a  
assignment c x = Var (C.toLower c, x) 

l3t = assignment

(|=) :: Char -> a -> Var a
(|=) = assignment

data ArExV = Atom  (Var Int) 
           | Plus  ArExV ArExV
	   | Times ArExV ArExV 
	   | Neg   ArExV
	
-- pretty printer
pretty :: ArExV -> String
pretty (Atom (Var (c,_))) = show c 
pretty (Plus t t')        = "(" ++ pretty t ++ "+" ++ pretty t' ++ ")" 
pretty (Times t t')       = "(" ++ pretty t ++ "*" ++ pretty t' ++ ")" 
pretty (Neg t)            = "-" ++ pretty t 

-- eval evaluates an expression and returns its value
-- eval :: ?
eval = undefined

