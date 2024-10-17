module MyInteger where

import {-qualified-} Nat    
import qualified Prelude as P  

data Int = Int Nat Nat

instance P.Show Int where 
  show (Int n m) = "(" P.++ P.show n P.++ ", " P.++ P.show m P.++ ")"

instance P.Eq Int where 
  Int n m == Int x y = n <+> y P.== x <+> m 

x = Int (S O) O
y = Int (S P.$ S O) (S O) 
z = Int O O
