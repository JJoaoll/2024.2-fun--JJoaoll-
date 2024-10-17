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

abs :: Int -> Int
abs x  = P.undefined 

times :: Int -> Int -> Int 
x `times` y = P.undefined 

x = Int (S O) O
y = Int (S P.$ S O) (S O) 
z = Int O O
a = Int 2 8 
b = Int 9 3 


