module ExNat where



import Prelude
    ( Show(..)
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

instance Show Nat where
    show O     = "O" 
    show (S n) = 'S':show n

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
n <+> S m = S n <+> m

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
        | otherwise = fromInteger (x - 1)

-- quotient
(</>) :: Nat -> Nat -> Nat
n </> m = if n >= m
          then S ((n - m) </> m) 
          else O

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)

absDiff :: Nat -> Nat -> Nat
absDiff n m = mmax(n, m) <-> mmin(n, m)

(|-|) = absDiff

fact :: Nat -> Nat
fact O  =  S O
fact (S n) = n * fact n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg _ = 1

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat x = if x <= 0 
          then O  
          else toNat (x - 1)

fromNat :: Integral a => Nat -> a
fromNat = undefined


