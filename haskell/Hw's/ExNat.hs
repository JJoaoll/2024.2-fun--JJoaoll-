{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude as P
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    , Maybe(..)
    , id
    ) 

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

n = S $ S $ S $ S $ S O 
m = S $ S $ S O
----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
  show n = show (fromNat n)
  {-  show O     = "O" 
    show (S n) = 'S':show n -}

instance Eq Nat where
    n == m 
      | (n <= m) && (m <= n) =  True 
      | otherwise = False
{-  
    O == O     = True 
    S n == S m = n == m
    _ == _     = False 
-}
instance Ord Nat where
    S n <= S m = n <= m
    S n <= O   = False 
    _ <= _     = True 

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

min :: Nat -> Nat -> Nat 
min n O         = O   
min O m         = O 
min (S n) (S m) = S (ExNat.min n m)

max :: Nat -> Nat -> Nat     
max n O         = n   
max O m         = m 
max (S n) (S m) = S (ExNat.max n m)



----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True 
isZero _ = False 

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Maybe Nat
pred O     = Nothing 
pred (S n) = Just n

even :: Nat -> Bool
even O     = True 
even (S n) = odd n 

odd :: Nat -> Bool
odd O     = False 
odd (S n) = even n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O     = n 
n <+> (S m) = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
S n <-> S m = n <-> m 
n <-> _     = n 

-- multiplication
(<*>) :: Nat -> Nat -> Nat
O <*> m = O 
S n <*> m = m <+> (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
n <^> O   = S O 
n <^> S m = n <*> (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
n </> O = error "cant divide by 0"
n </> m = if n >= m 
          then S (n - m) </> m 
          else O

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> O = error "cant divide by 0"
n <%> m = if n >= m 
          then (n - m) <%> m 
          else n

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = m <%> m == O

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
n `absDiff` m = ExNat.max n m <-> ExNat.min n m 

(|-|) = absDiff

factorial :: Nat -> Nat
factorial O     = S O 
factorial (S n) = n <*> factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O 
sg _ = 1 

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _     = error "nao ha log de 0"
lo _ O     = error "nao ha log pra base 0"
lo _ (S O) = error "nao ha log para base SO"
 
lo n m = if n >= m 
          then S (lo n' m) 
          else O 
  where n' = n </> m 


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat x = if x > 0 
          then S (toNat x)
          else O

fromNat :: Integral a => Nat -> a
fromNat O     = 0 
fromNat (S n) = 1 + fromNat n

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where
    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs = id 
    signum = sg
    fromInteger x
      | x <= 0     = O 
      | otherwise = S (fromInteger (x - 1))

