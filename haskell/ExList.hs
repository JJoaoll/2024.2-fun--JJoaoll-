module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import Nat 

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> P.Maybe a
head []     = P.Nothing 
head (x:_) = P.Just x 

tail :: [a] -> [a]
tail []     = [] 
tail (_:xs) = xs 

null :: [a] -> Bool
null [] = True 
null _  = False

length :: Integral i => [a] -> i
length [] = 0 
lenght (x:xs) = 1 + lenght xs

sum :: Num a => [a] -> a
sum []     = 0 
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1 
product (x:xs) = x * product xs

append :: a -> [a] -> [a]
append x [] = [x]
append x (y:ys) = y:(append x ys) 

reverse :: [a] -> [a]
reverse [] = [] 
reverse (x:xs) = append x (reverse xs)

(++) :: [a] -> [a] -> [a]
xs ++ []     = xs
xs ++ (y:ys) = append y xs ++ ys

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x []     = [x]
snoc x (y:ys) = y:(snoc x ys)

flip :: (a -> b -> c) -> (b -> a -> c) 
flip f x y = f y x
-- flip f = \x -> \y -> f y x

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
-- ??????????????
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum []     = error "this list is empty"
minimum [x]    = x 
minimum (x:xs) = P.min x (minimum xs)
-- minimum (x':x'':xs) = minimum (x:xs) where x = P.min x' x''

maximum :: Ord a => [a] -> a
maximum []     = error "this list is empty"
maximum [x]    = x 
maximum (x':x'':xs) = maximum (x:xs) where x = P.max x' x''

take :: Nat -> [a] -> [a] 
take _ []         = []
take O _          = [] 
take (S n) (x:xs) = x : take n xs

drop :: Nat -> [a] -> [a] 
drop _ []         = []
drop O xs         = xs 
drop (S n) (_:xs) = drop n xs 


takeWhile :: (a -> Bool) -> [a] -> [a]
takewhile _ []     = [] 
takeWhile f (x:xs) = if f x 
                     then x : takeWhile f xs 
                     else []


dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = [] 
dropWhile f (x:xs) = if f x 
                     then dropWhile f xs 
                     else x:xs


fromWhile :: Nat -> (a -> Bool) -> [a] -> [a]
-- fromWhile n p xs = takeWhile p (drop n xs)
fromWhile n p = takeWhile p . drop n 

fromFor :: Nat -> Nat -> [a] -> [a]
fromFor n m = take m . drop n

fromTo :: Nat -> Nat -> [a] -> [a]
fromTo n m = take (S (m - n)) . drop n 

fromToThat :: Nat -> Nat -> (a -> Bool) -> [a] -> [a]
fromToThat n m p = filter p . fromTo n m 

tails :: [a] -> [[a]] 
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs

init :: [a] -> [a] 
init []     = error "empty list" 
init [x]    = [] 
init (x:xs) = x : init xs 

inits :: [a] -> [[a]]
inits []     = [[]] 
inits xs     = append xs (inits (init xs)) 

stretch :: Nat -> [a] -> [a] 
stretch _ []     = [] 
stretch n (x:xs) = replicate n x ++ stretch n xs 
-- subsequences

countdown :: Nat -> [Nat] 
countdown O     = [O]
countdown (S n) = S n : countdown n 

firstWith :: (a -> Bool) -> [a] -> P.Maybe a 
firstWith p = head . filter p

conjall :: [Bool] -> Bool 
conjall = fold (&&) True 

disists :: [Bool] -> Bool 
disists = fold (||) False

any :: (a -> Bool) -> [a] -> Bool 
any p = disists . map p

all :: (a -> Bool) -> [a] -> Bool 
all p = conjall . map p

and = conjall
or  = disists

concat :: [a] -> [a] -> [a]
concat xs []     = xs 
concat xs (y:ys) = concat (xs <: y) ys 

elem :: Eq a => a -> [a] -> Bool 
elem x = any (== x) 

elem2 :: Eq a => a -> [a] -> Bool
elem2 x []     = False 
elem2 x (y:ys) = 
  case x == y of 
  True  -> True 
  False -> elem2 x ys 
-- (without using other functions except (==))


(!!) :: [a] -> Nat -> a 
xs !! O         = case head xs of    
                  P.Just x -> x 
                  P.Nothing -> error "ai foi dms, bixo"
(x:xs) !! (S n) = xs !! n

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter f (x:xs) = if f x 
                  then x : filter f xs 
                  else filter f xs

map :: (a -> b) -> [a] -> [b] 
map _ []     = [] 
map f (x:xs) = f x : map f xs

copy :: [a] -> [a] 
copy xs = map P.id xs

cycle :: [a] -> [a] 
cycle [] = error "empty list"
cycle xs = (copy xs) ++ cycle xs
-- repeat
replicate :: Nat -> a -> [a]
replicate O _     = []
replicate (S n) x = x : replicate n x
{-  
range :: Nat -> Nat -> [Nat] 
range n m 
  | n == m = [m] 
  | n < m  = n : range (S n) m  
  | n > m  = n : range (pred n) m   
    -}           

-- versao bem
fold :: (a -> a -> a) -> a -> [a] -> a 
fold _ e []     = e 
fold f e (x:xs) = f x (fold f e xs)  



-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- pi 

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

