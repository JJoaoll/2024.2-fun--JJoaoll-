module Logic where

import qualified Data.Char as C


data Formula where
   F       :: Formula 
   T       :: Formula
   Var     :: Char -> Formula 
   And     :: Formula -> Formula -> Formula
   Or      :: Formula -> Formula -> Formula
   Implies :: Formula -> Formula -> Formula
  deriving (Eq) 

instance (Show Formula) where 
	show F              = "F"
	show T              = "T"
	show (Var p)        = [C.toUpper p] 
	show (And f f')     = show f ++ " ^ " ++ show f' 
	show (Or f f')      = show f ++ " v " ++ show f' 
	show (Implies f f') = show f ++ " => " ++ show f'


(&)   = And 
(==>) = Implies
ou = Or





