module Origami where


import ExList (tails, inits)
import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , scanl, scanr
    , sum , product
    , minimun, maximum
    , length
    , concat
    , filter
    , map, reverse
    , any , all
    , and , or
    , takeWhile , dropWhile
    )

import qualified Prelude as P

--
-- define the following folds:
--

-- let's define a type tester:
contest :: String -> String -> String
""   `contest` str2 = "nil" `contest` str2
str1 `contest` ""   = str1  `contest` "nil"
str1 `contest` str2 = "(" ++ str1 ++ " # " ++ str2 ++ ")"

cons :: a -> [a] -> [a]
cons x xs = x : xs

-- foldl ([a] -> a -> [a]) -> [a] -> [a] -> [a]
snoc :: a -> [a] -> [a]
snoc x = foldl (flip cons) [x]

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ e []        = e
foldr op e (x : xs) = x `op` foldr op e xs

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ e []        = e
foldl op e (x : xs) = foldl op e xs `op` x


-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ []        = error "empty list"
foldr1 _ [x]       = x
foldr1 op (x : xs) =
  let x' = foldr1 op xs
  in (x `op` x')

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ []        = error "empty list"
foldl1 _ [x]       = x
foldl1 op (x : xs) =
  let x' = foldl1 op xs
  in (x' `op` x)

--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl op e = map (foldl op e) . inits
-- scanl op e []       = [e]
-- scanl op e (x : xs) = e : ys'
--   where ys' = [y `op` x | y <- scanl op e xs]

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr op e = map (foldr op e) . tails

--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum = foldl (+) 0

product :: Num a => [a] -> a
product = foldl (*) 1

-- listas infinitas, bby
concat :: [[a]] -> [a]
concat = foldl (flip (++)) []
-- concat = foldr (++) []

-- valido?
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

and :: [Bool] -> Bool
and = foldl (&&) True

or :: [Bool] -> Bool
or  = foldl (||) False

minimum :: Ord a => [a] -> a
minimum = foldl1 min

maximum :: Ord a => [a] -> a
maximum = foldl1 max

length :: Integral i => [a] -> i
length = foldr (\x y -> y + 1) 0

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x -> if p x then (x:) else id) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (cons . f) []
-- map f = foldl (flip $ cons . f) []

-- fold (b -> a -> b) -> b -> [a] -> b
-- fold (([a] -> a -> [a]) -> [a])) -> [a] -> [a]

reverse :: [a] -> [a]
reverse = foldl (flip snoc) []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x -> if p x then (x:) else const []) [] 

-- sorry
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile p = foldr (\x -> if p x then const [] else )) [] 

revFilter :: (a -> Bool) -> [a] -> [a]
revFilter p = foldr (\x -> if p x then id else (x:)) [] 

-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: Integral i => [i] -> (i, Maybe i)
semo = undefined

-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: Eq a => [a] -> [a]
remdups = undefined

safeLast :: [a] -> Maybe a
safeLast = undefined

-- dec2int [1,9,9,2] = 1992
dec2int :: Integral i => [i] -> i
dec2int = undefined




