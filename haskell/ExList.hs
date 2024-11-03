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

-- ASSUMPTION: xs and yes are sorted
merge :: Ord a => [a] -> [a] -> [a]
merge xs []                 = xs
merge [] ys                 = ys
merge (xs'@(x:xs)) (ys'@(y:ys)) 
  | x <= y = x:merge xs ys'
  | y < x  = y:merge xs' ys

--mSort :: Ord a => [a] -> [a] 
--mSort  


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
inits xs     = (inits (init xs)) <: xs 

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

mRepeat :: [a] -> [[a]] 
mRepeat xs = xs : mRepeat xs

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

-- TO-REDO
fold :: (a -> a -> a) -> a -> [a] -> a 
fold _ e []     = e 
fold f e (x:xs) = f x (fold f e xs)  

isPrefixOf :: Eq a => [a] -> [a] -> Bool 
isPrefixOf [] ys         = True 
isPrefixOf xs []         = False 
isPrefixOf (x:xs) (y:ys) = (x == y) && isPrefixOf xs ys 
-- isInfixOf
--

{-
isSuffixOf :: Eq a => [a] -> [a] -> Bool 
isSuffixOf [] ys         = True 
isSuffixOf xs []         = False 
isSuffixOf (x:xs) ys = (x == (rear ys)) && isSuffixOf xs (cutRear ys) 

-}

{-
rear :: [a] -> a 
rear []     = error "empty list" 
rear [x]    = x
rear (x:xs) = x:(rear xs) 

safeRear :: [a] -> P.Maybe a 
safeRear []     = P.Nothing 
safeRear [x]    = P.Just x
safeRear (x:xs) = P.Just (rear xs)

cutRear :: [a] -> [a]
cutRear (x:xs) = x:cutRear xs
cutRear _      = []

-}
zip :: [a] -> [b] -> [(a, b)]
zip [] _          = [] 
zip _ []          = [] 
zip (x:xs) (y:ys) = (x, y): zip xs ys 

muncurry :: (a -> b -> c) -> (a, b) -> c
muncurry f (x, y) = f x y

mcurry :: ((a, b) -> c) -> (a -> b -> c) 
mcurry f a b = f (a,b)
{-
zipWithCurried :: (a -> b -> c) -> ([a], [b]) -> [c]
zipWithCurried f = map (uncurry f) . (mcurry zip)

zipWithLegal :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWithLegal f xs = map (uncurry f) . (zip xs) 

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWith _ [] _          = [] 
zipWith _ _ []          = [] 
zipWith f (x:xs) (y:ys) = (f x y):zipWith f xs ys
-}

-- intercalate
-- nub

oneToR :: ([a], [a]) -> ([a], [a])
oneToR (xs, ys) = case xs of 
                  []    -> (xs, ys) 
                  x:xs' -> (xs',x:ys)


oneToL :: ([a], [a]) -> ([a], [a])
oneToL (xs, ys) = case ys of 
                  []    -> (xs, ys) 
                  y:ys' -> (xs<:y,ys')


-- aqui o "<'>" significa "decaptado"
itr :: (a -> a) -> Nat -> (a -> a)
itr _ O x     = x
itr f (S n) x = f (ExList.itr f n x) 
 
splitAt :: Nat -> [a] -> ([a], [a])
-- splitAt n = (ExList.itr oneToL n) . ([], )

--splitAt [] _         = ([], [])
splitAt O xs         = ([], xs) 
splitAt (S n) (xs) = let (ls, rs) = (splitAt n xs) 
                     in case rs of 
                          []      -> (ls, rs)
                          (r:rs') -> (ls<:r,rs')
                    


--splitAt (xs'@(x:xs)) (S n) = let (ls, rs) = (x:thing,  


--splitAt n xs = case n of 
--               O    -> ([], xs) 
--               S n' -> case xs of 
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

