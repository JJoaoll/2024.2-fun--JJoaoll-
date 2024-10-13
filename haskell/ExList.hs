module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

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
head (x:xs) = P.Just x 

tail :: [a] -> [a]
tail []     = [] 
tail (x:xs) = xs 

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
minimum (x':x'':xs) = minimum (x:xs) where x = P.min x' x''

maximum :: Ord a => [a] -> a
maximum []     = error "this list is empty"
maximum [x]    = x 
maximum (x':x'':xs) = maximum (x:xs) where x = P.max x' x''

-- take
-- drop

-- takeWhile
-- dropWhile

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

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

