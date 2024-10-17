module MyInt where

import {-qualified-} Nat    
import qualified Prelude as P  

data Int = Int Nat Nat

instance P.Show Int where
  show (Int n m) = if n P.<= m 
                   then P.show (n |-| m)
                   else '-':P.show (n |-| m)

  --show (Int n m) = "(" P.++ P.show n P.++ ", " P.++ P.show m P.++ ")"

instance P.Eq Int where 
  Int n m == Int x y = n <+> y P.== x <+> m

plus :: Int -> Int -> Int 
Int n m `plus` Int x y = Int (n <+> x) (m <+> y) 

minu :: Int -> Int 
minu (Int n m) = Int m n 

minus :: Int -> Int -> Int 
x `minus` y = x `plus` minu y 

sgn :: Int -> Int  
sgn (Int n m)  
  | n P.< m     = Int O (S O) 
  | n P.== m    = Int O O 
  | P.otherwise = Int (S O) O

isNeg :: Int -> P.Bool
isNeg x = sgn x P.== Int (S O) O  
 
isNotNeg :: Int -> P.Bool 
isNotNeg = P.not . isNeg 

isPos :: Int -> P.Bool 
isPos x = sgn x P.== Int O (S O)

isNotPos :: Int -> P.Bool 
isNotPos = P.not P.(.) isPos

haveShameSgn :: Int -> Int -> P.Bool 
haveShameSgn x y = (isNotNeg x P.&& isNotNeg y) 
                  P.|| (isNotPos x P.&& isNotPos y)

abs :: Int -> Int
abs x
  | sgn x P.== Int (S O) O = minu x 
  | P.otherwise                 = x 
 
times :: Int -> Int -> Int 
x `times` y
  | haveShameSgn x y = P.undefined 
  | P.otherwise      = P.undefined 
              

x = Int (S O) O
y = Int (S P.$ S O) (S O) 
z = Int O O
a = Int 2 8 
b = Int 9 3 


