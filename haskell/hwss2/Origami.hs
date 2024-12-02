module Origami where

import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , scanl, scanr
    , sum , product
    , length
    , concat
    , filter
    , map
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

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr _ e []        = e 
foldr op e (x : xs) = x `op` foldr op e xs 

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b -> a -> b) -> b -> [a] -> b 
foldl _ e []        = e  
foldl op e (x : xs) = foldl op e xs `op` x


-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> Maybe a     
foldr1 _ []        = Nothing 
foldr1 _ [x]       = Just x
foldr1 op (x : xs) = 
  let Just x' = foldr1 op xs 
  in  Just (x `op` x') 

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> Maybe a
foldl1 _ []        = Nothing                     
foldl1 _ [x]       = Just x 
foldl1 op (x : xs) = 
  let Just x' = foldl1 op xs 
  in  Just (x' `op` x)

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
scanl op e []       = [e]
scanl op e (x : xs) = e : xs'
  where xs' = [y `op` x | y <- scanl op e xs]

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr op e []       = [e]
scanr op e (x : xs) = xs' ++ [e]
  where xs' = [x `op` y | y <- scanr op e xs]

--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum = undefined

product :: Num a => [a] -> a
product = undefined

concat :: [[a]] -> [a]
concat = undefined

any :: (a -> Bool) -> [a] -> Bool
any = undefined

all :: (a -> Bool) -> [a] -> Bool
all = undefined

and :: [Bool] -> Bool
and = undefined

or :: [Bool] -> Bool
or = undefined

minimum :: Ord a => [a] -> a
minimum = undefined

maximum :: Ord a => [a] -> a
maximum = undefined

length :: Integral i => [a] -> i
length = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

reverse :: [a] -> [a]
reverse = undefined

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = undefined

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
