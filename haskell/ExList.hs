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

len :: [a] -> Nat
len []     = O 
len (x:xs) = S (len xs)

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

sorted :: Ord a => [a] -> Bool
sorted []         = True 
sorted xs'@(_:xs) = conjall (L.zipWith (<=) xs' xs)

--filter usandao concat
-- deve funfar se usar mapMaybe seguida de 
-- toList. 




-- ASSUMPTION: xs and yes are sorted
merge :: Ord a => [a] -> [a] -> [a]
merge xs []                 = xs
merge [] ys                 = ys
merge (xs'@(x:xs)) (ys'@(y:ys)) 
  | x <= y = x:merge xs ys'
  | y < x  = y:merge xs' ys

mSort :: Ord a => [a] -> [a] 
mSort []  = [] 
mSort [x] = [x]
mSort xs = let (ls, rs) = (halfa xs)
           in merge (mSort ls) (mSort rs)

halfa :: [a] -> ([a], [a])
halfa xs = splitAt (len xs </> 2) xs 

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

subsequences :: [a] -> [[a]]
subsequences []         = [[]] 
subsequences (x:xs) = pput x (subsequences xs)
                where pput = \x yss -> case yss of
                               []       -> []
                               (ys:yss) -> ys:(x:ys):(pput x yss)
{-
			 let yss = subsequences xs 
                         in x `bb` yss
	where x `bb` []     = []
	      x `bb` (ys:yss) = ys:(x:ys):(x `bb` yss) 
-}
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

concat :: [[a]] -> [a]
concat = fold (++) [] 

concatWith :: a -> [[a]] -> [a]
concatWith x = fold (putInTheMiddle x) []
  where putInTheMiddle x xs xs' = case xs' of
                                  []     -> xs
                                  (_:_)  -> xs ++ [x] ++ xs' 

elem :: Eq a => a -> [a] -> Bool 
elem x = any (== x) 

elem2 :: Eq a => a -> [a] -> Bool
elem2 x []     = False 
elem2 x (y:ys) = 
  case x == y of 
  True  -> True 
  False -> elem2 x ys 
-- (without using other functions except (==))

-- isso ta bugado
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

isSuffixOf :: Eq a => [a] -> [a] -> Bool 
isSuffixOf [] ys         = True 
isSuffixOf xs []         = False 
isSuffixOf xs ys'@(y:ys) = xs == ys' || xs `isSuffixOf` ys 


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

--zipWithCurried :: (a -> b -> c) -> ([a], [b]) -> [c]
--zipWithCurried f = map (uncurry f) . (mcurry zip)

zipWithLegal :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWithLegal f xs = map (uncurry f) . (zip xs) 

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWith _ [] _          = [] 
zipWith _ _ []          = [] 
zipWith f (x:xs) (y:ys) = (f x y):zipWith f xs ys


isIn :: Eq a => a -> [a] -> Bool
isIn x = disists . map (==x)


intercalate :: [a] -> [[a]] -> [a]
-- intercalate xs (ys:yss@(_:_)) = ys ++ xs ++ (intercalate xs yss)
-- intercalate _ yss = fold (++) [] yss 
intercalate _ []        = []  
intercalate _ [ys]      = ys 
intercalate xs (ys:yss) = ys ++ xs ++ (intercalate xs yss)


nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs)   
  | x `isIn` xs = nub xs
  | otherwise   = x:nub xs

sings :: [a] -> [[a]] 
sings [] = [] 
sings (x:xs) = [x]:sings xs

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
splitAt O xs       = ([], xs) 
splitAt (S n) (xs) = let (ls, rs) = (splitAt n xs) 
                     in case rs of 
                          []      -> (ls, rs)
                          (r:rs') -> (ls<:r,rs')
                    
-- nameMe :: [a] -> 

put :: a -> [a] -> Nat -> [a]
put x [] _               = [x] 
put x ys O               = x:ys
put x (y:ys) (S n) = y:(put x ys n) 

-- Put EveryWhere
putEw :: a -> [a] -> [[a]] -- aqui embaixo tbm daria pra usar o countdown
putEw x ys = [put x ys n | n <- [O .. len ys]]
--putEw x ys = map (put x ys) [O .. (len ys)]
            
putInAll :: a -> [[a]] -> [[a]] 
putInAll _ []           = [] 
putInAll x [xs]         = [(put x xs $ S O),(put x xs O)] 
putInAll x (xs:xss) = put x xs (S $ len xss):putInAll x xss
-- como descrever isso com lets??
{-
putInAll x (xs:xss) = (put x xs O): case xss of 
                                    []       -> []
                                    (ys:yss) -> (put x ys (S O)) : case yss of 
                                                                   [] -> []
                                                                 (zs : zss) ...
-}



pi :: [a] -> [[a]]
pi []     = [[]]
pi [x]    = [[x]]
--pi  _  = undefined
pi (x:xs) = concat [putEw x xs' | xs' <- pi xs]
--pi (x:xs) = concat (map (putEw x) xs')
--    where xs' = pi xs
             
--pi (x:xs) = concat $ map (putEw x) [xs] 


--splitAt (xs'@(x:xs)) (S n) = let (ls, rs) = (x:thing,  


--splitAt n xs = case n of 
--               O    -> ([], xs) 
--               S n' -> case xs of 
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break p xs'@(x:xs) 
  | p x       = ([], xs') 
  | otherwise = (x:ls,rs)
  where (ls, rs) = break p xs
break _ _     = ([], []) 

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy _ []     = ([], []) 
splitBy p (x:xs) 
  | p x       = (ls,x:rs) 
  | otherwise = (x:ls,rs)
  where (ls,rs) = splitBy p xs

pairs :: [a] -> [(a, a)]
pairs (x:xs'@(x':_)) = (x,x'):pairs xs'
pairs _ = []

pairsv2 = map (\(x,y) -> [x,y]) . pairs

separateBy :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
separateBy _ []     = []
separateBy p (x:xs) =
  case (separateBy p xs) of 

       [] ->
         if p x then []
                else [[x]]

       yss'@(ys:yss) -> 
         if p x then [] : yss' 
                else (x : ys) : yss

lines :: String -> [String]
lines = splitWhereFind '\n'

splitWhereFind :: (Eq a) => a -> [a] -> [[a]]
splitWhereFind x = separateBy (== x)

words :: String  -> [String]
words = splitWhereFind ' ' . filter (not . C.isControl)

-- as duas funcoes abaixo nem merecem esses nomes
unlines :: [String] -> String
unlines = concat . map (++ "\n")

unwords :: [String] -> String
unwords = concatWith ' '

type Text = String
type Word = String
commonWords :: Int -> Text -> String
commonWords n = concat  .  showRun  . take n  . countRuns  . sortWords  .  words  .  (map C.toLower)
 where sortWords = mSort 
       countRuns = toHashCount 

-- note que ate da pra definir a sings em termos desta
putAllIn :: [a] -> [[a]] -> [[a]]
putAllIn [] yss          = yss 
putAllIn (x:xs) yss      = (x : ys) : putAllIn xs yss' 
  where ys   = concat (take 1 yss)
        yss' = drop 1 yss 


transpose :: [[a]] -> [[a]] 
transpose []       = [] 
transpose (xs:xss) = putAllIn xs (transpose xss)

-- checks if the letters of a phrase form a palindrome (see below for examples)

padronize :: String -> String 
padronize = filter C.isLetter . map C.toLower

palindrome :: String -> Bool
palindrome str = str' == reverse str'
  where str' = padronize str

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

