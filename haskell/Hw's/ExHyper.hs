module ExHyper where

import Prelude hiding ( exp, pred )

-- Nat datatype --------------------------------

toInt :: Nat -> Integer 
toInt O     = 0
toInt (S n) = 1 + toInt n

data Nat = O | S Nat
     deriving (Eq)

instance (Show Nat) where 
    show n = show(toInt n)

instance (Num Nat) where
    (+) = add
    (*) = mul
    abs = id
    fromInteger 0 = O
    fromInteger n
      | n > 0     = S $ fromInteger (n-1)
      | otherwise = O
    signum O = O
    signum n = S O
    negate n = O

instance (Ord Nat) where
    O     <= m     = True
    (S n) <= O     = False
    (S n) <= (S m) = n <= m

{- explicit definitions of add, mul, exp:

add n O     = n
add n (S m) = S (add m n)

mul n O     = O
mul n (S m) = add (mul n m) n

exp n O     = S O
exp n (S m) = mul (exp n m) n

-}

------------------------------------------------

-- substitute 'undefined' by the correct number
-- to define each of those functions:

nextLvl ::(a -> a -> a) -> a -> (a -> a -> a) 
nextLvl (><) e x y = undefined 
 
add :: Nat -> Nat -> Nat
add n O     = n  
add n (S m) = S (add n m)

mul :: Nat -> Nat -> Nat
mul n O     = O 
mul n (S m) = n + (mul n m)  

exp :: Nat -> Nat -> Nat
exp _ O     = S O 
exp n (S m) = n `mul` exp n m 

nextLv :: (Nat -> Nat -> Nat) -> Nat -> (Nat -> Nat -> Nat)
nextLv f e n O    = e
nextLv f e n (S m)  = n `f` (nextLv f e n m)

-- hyper n should return the n'th operation in the sequence:
-- (..?..), add, mul, exp, ...?

toNat:: Integral i => i -> Nat
toNat x 
  | x <= 0    = O 
  | otherwise = S (fromIntegral (x - 1))

pred :: Nat -> Nat 
pred O     = O 
pred (S n) = n



replct :: Nat -> a -> [a]
replct O _     = []
replct (S n) x = x : replct n x

pw :: (Nat -> Nat -> Nat) -> Nat -> [Nat] -> Nat 
pw _ id []     = id  
pw f id (x:xs) = f x (pw f id xs) 

h = hyper 


itr :: (Nat -> Nat) -> Nat -> Nat -> Nat
itr f O m     = m 
itr f (S n) m = f (itr f n m)  

hyper :: Integral i => i -> (Nat -> Nat -> Nat)
hyper 0 n m = itr S n m
hyper 1 n m = itr (hyper 0 n) m n
hyper x n m = itr (hyper (x-1) n) m (S O) 
--hyper 1 n = add 
--hyper x = nextLv (hyper (x - 1)) (toNat (x - 2))

{-  -}--hyper 2 n m = pw  (hyper 1) (S O) (replct n m)
-- S $ (itr (hyper (y - 1) n) n m)      

