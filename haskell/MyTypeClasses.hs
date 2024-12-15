module MyTypeClasses where
import Prelude  
import qualified ExList as L

class SemiGroup a where 
--opSG has to be Associative 
  opSG :: a -> a -> a 

instance Num String where 
  (+)    = (++)
  (*)    = (++) 
  abs    = id 
  signum = id 
  fromInteger _ = ""
  negate = id 


len :: Num b => [a] -> b
len [] = 0 
len (_ : xs) = 1 + len xs

compare :: SemiGroup a => a -> a -> a -> (a, a)
compare x y z = (opSG x y, opSG x z)

class SemiGroup a => Monoid a where
  opM :: a -> a -> a
  opM = opSG 
--e has to be the opM-id 
  e :: a

{-
class Monoid a => Group a where 
  (*) :: a -> a -> a
  (*) = opSG
  u :: a
  u = e 
--inv a has to return the opG inv 
  inv :: a -> a 
-}

{-
class Group a => AbelianGroup a where 
--the (+) is commutates 
 (+) :: a -> a -> a 
 (+) = (*) 
 z :: a 
 z = u 
 (-) :: a -> a 
 (-) = inv 
-}

class Functor f where
  fmap :: (a -> b) -> ((f a) -> (f b)) 
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g 
 
-- instance (Functor []) where 
--   fmap = L.map

