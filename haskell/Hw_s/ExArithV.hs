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

data ArExV = Atom  (Int) 
           | V     (Var Int)
           | Plus  ArExV ArExV
	   | Times ArExV ArExV 
	   | Neg   ArExV
	
-- pretty printer
pretty :: ArExV -> String
pretty (Atom x)           = show x 
pretty (V (Var (c,_)))    = show c   
pretty (Plus t t')        = "(" ++ pretty t ++ " + " ++ pretty t' ++ ")" 
pretty (Times t t')       = "(" ++ pretty t ++ " * " ++ pretty t' ++ ")" 
pretty (Neg t)            = "-(" ++ pretty t ++ ")" 

-- eval evaluates an expression and returns its value
eval :: ArExV 
eval = undefined -- facil.


-- example expressions
ex1 = (Atom 23) `Plus` (V (Var  ('x', 3)))  
ex2 = ex1 `Times` ((Atom 7) `Plus` ((V (Var ('y', 6)))) `Times` (Atom 8))
ex3 = Times ex1 ex2
ex4 = Neg $ ex3 `Plus` ex1
ex5 = (Neg ex1) `Times` (Neg ex4)


