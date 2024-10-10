module ExNat where

import Prelude
    ( Int
    , Show(..)
    , Eq(..)
    , Ord(..)
    , Num(.. )
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
   ) 

data Nat = O | S Nat

n = S $ S $ S O 
m = S $ S O

toInt :: Nat -> Int

toInt O     = 0 
toInt (S n) = 1 + toInt n

instance Show Nat where
    show n =  show (toInt n)
    --show O     = "O" 
    --show (S n) = 'S':show n

instance Eq Nat where
    O == O         = True  
    (S n) == (S m) = n == m
    _ == _         = False

instance Ord Nat where
   O <= m   = True 
   S n <= O = False
   S n <= S m = n <=m

minn :: Nat -> Nat -> Nat 
minn O m         = O 
minn n O         = O  
minn (S n) (S m) = S (minn n m)


mmin :: (Nat, Nat)  -> Nat
mmin (n, O)     = O 
mmin (O, m)     = O 
mmin (S n, S m) = S (mmin (n, m))

mmax :: (Nat, Nat)  -> Nat
mmax (n, O)     = n 
mmax (O, m)     = m 
mmax (S n, S m) = S (mmax (n, m))

isZero :: Nat -> Bool
isZero O = True 
isZero _ = False

pred :: Nat -> Nat
pred O     = O 
pred (S n) = n

even :: Nat -> Bool
even O     = True 
even (S n) = odd n 

odd :: Nat -> Bool
odd O     = False 
odd (S n) = even n

(<+>) :: Nat -> Nat -> Nat
n <+> O     = n 
n <+> S m = S (n <+> m)

(<->) :: Nat -> Nat -> Nat
n <-> O = n 
O <-> m = O 
S n <-> S m = n <-> m

(<*>) :: Nat -> Nat -> Nat
O <*> m   = O 
S n <*> m = m <+> (n <*> m)


(<^>) :: Nat -> Nat -> Nat
n <^> O   = S O 
n <^> S m = n <*> (n <^> m)

instance Num Nat where
    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = O 
        | x == 0    = O
        | otherwise = S (fromInteger (x - 1))

(</>) :: Nat -> Nat -> Nat
n </> m = if n >= m
          then S (n' </> m) 
          else O
  where n' = n <-> m

(<%>) :: Nat -> Nat -> Nat
n <%> m = if n >= m 
          then n' <%> m 
          else n
  where n' = n <-> m

(<|>) :: Nat -> Nat -> Bool
n <|> m = case n <%> m of 
              O -> True 
              _ -> False
divides = (<|>)

absDiff :: Nat -> Nat -> Nat
absDiff n m = mmax(n, m) <-> mmin(n, m)

(|-|) = absDiff

fact :: Nat -> Nat
fact O  =  S O
fact (S n) = n * fact n

sg :: Nat -> Nat
sg O = O
sg _ = 1

log :: Nat -> Nat -> Nat
log O _     = error "nao ha log de 0"
log _ O     = error "nao ha log pra base 0"
log _ (S O) = error "nao ha log para base SO"
 
log n m = if n >= m 
         then S (log n' m) 
         else O 
  where n' = n </> m 

toNat :: Integral a => a -> Nat
toNat x = if x <= 0 
          then O  
          else toNat (x - 1)

fromNat :: Integral a => Nat -> a
fromNat O     = 0
fromNat (S n) = 1 + fromNat n



-- abbrevs (syntactic sugar) to the 50 first Nat`s :PPP

o    = O
so   = S o
sso  = S so
ssso = S sso
sssso = S ssso
ssssso = S sssso
sssssso = S ssssso
ssssssso = S sssssso
sssssssso = S ssssssso
ssssssssso = S sssssssso
sssssssssso = S ssssssssso
ssssssssssso = S sssssssssso
sssssssssssso = S ssssssssssso
ssssssssssssso = S sssssssssssso
sssssssssssssso = S ssssssssssssso
ssssssssssssssso = S sssssssssssssso
sssssssssssssssso = S ssssssssssssssso
ssssssssssssssssso = S sssssssssssssssso
sssssssssssssssssso = S ssssssssssssssssso
ssssssssssssssssssso = S sssssssssssssssssso
sssssssssssssssssssso = S ssssssssssssssssssso
ssssssssssssssssssssso = S sssssssssssssssssssso
sssssssssssssssssssssso = S ssssssssssssssssssssso
ssssssssssssssssssssssso = S sssssssssssssssssssssso
sssssssssssssssssssssssso = S ssssssssssssssssssssssso
ssssssssssssssssssssssssso = S sssssssssssssssssssssssso
sssssssssssssssssssssssssso = S ssssssssssssssssssssssssso
ssssssssssssssssssssssssssso = S sssssssssssssssssssssssssso
sssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssssssssssso

