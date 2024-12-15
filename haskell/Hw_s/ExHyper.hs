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

itr0 :: (Nat -> Nat) -> Nat -> Nat -> Nat
itr0 f O m     = m 
itr0 f (S n) m = f (itr0 f n m)  

itrU :: (Nat -> Nat) -> Nat -> Nat -> (Nat -> Nat)  
itrU _ O e n     = e
itrU f (S O) e n = n 
itrU f (S k) e n = f (itrU f k e n)



fold :: (a -> a -> a) -> a -> [a] -> a 
fold _ e []     = e 
fold f e (x:xs) = f x (fold f e xs)  

repl :: Nat -> a -> [a]
repl O _     = []
repl (S n) x = x : repl n x


hyper :: Integral i => i -> (Nat -> Nat -> Nat)
--zero-orio
hyper 0 n m = O  

-- Successorio
hyper 1 n m = 
  case n of 
    O    -> m   
    S n' -> hyper 1 n' (S m)

-- Somatorio
hyper 2 n m = fold (hyper 1) (hyper 0 (S O) O) (repl m n)
 
-- Produtorio 
hyper 3 n m = fold (hyper 2) (hyper 1 (S O) O) (repl m n)

-- ???
hyper x n m = fold (hyper (x-1)) (hyper (x-2) (S O) O) (repl m n)


{-hyper 2 n m =
  case n of 
    O    -> O 
    S n' -> hyper 2 n' (hyper 1 n m)
-}
-- hyper x n m = itr (hyper (x-1) n) m (S O) 
--hyper 1 n = add 
--hyper x = nextLv (hyper (x - 1)) (toNat (x - 2))

{-  -}--hyper 2 n m = pw  (hyper 1) (S O) (replct n m)
-- S $ (itr (hyper (y - 1) n) n m)      

