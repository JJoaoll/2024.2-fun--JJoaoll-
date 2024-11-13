module ExSet where {-
    ( Set
    , empty
    , singleton
    , fromList
    , toList
    , powerSet
    , insert
    , delete
    , member
    , notMember
    , null
    , size
    , isSubsetOf
    , isProperSubsetOf
    , disjoint
    , pairwiseDisjoint
    , union
    , inter
    , (\\)
    , unions
    , inters
    , cartesianProduct
    , disjointUnion
    , filter
    , partition
    , map
    ) where
-}
import Prelude hiding  (map) 
import qualified Data.List as L 
--import qualified Data.List as L (map) 
import ExList (isIn)
-- import ExList (isIn, nub, subsequences) 

data Set a = (Eq a, Show a) =>  Set [a]

-- CAUTION: you may need to add constraints to your types and instances!

areIn :: Eq a => [a] -> [a] -> Bool
areIn [] ys     = True 
areIn (x:xs) ys = (x `isIn` ys) && (xs `areIn` ys)


instance Eq (Set a) where
    Set xs == Set ys = (xs `areIn` ys) && (ys `areIn` xs)

instance Show (Set a) where
    show (Set [])     = "{}"
    show (Set xs'@(_:_)) = let printAll = \xs -> case xs of
                                          []           -> " "
					  [x]          -> show x 
					  (x:xs@(_:_)) -> (show x) ++ ", " ++ printAll xs
                           in "{" ++ printAll (L.nub xs') ++ "}"


-- smart constructor

set :: (Eq a, Show a) => [a] -> Set a 
set = Set . L.nub
--set xs = Set xs

isEmpty :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _        = False

empty :: (Eq a, Show a) => Set a 
empty = Set [] 

singleton :: (Eq a, Show a) => a -> Set a
singleton = Set . (:[])

fromList :: (Eq a, Show a) => [a] -> Set a
fromList = set 

toList :: Set a -> [a]
toList (Set xs) = xs

powerSet :: (Eq a, Show a) => Set a -> Set (Set a)
powerSet = set . (L.map set) . L.subsequences . toList 

insert :: (Eq a, Show a) => a -> Set a -> Set a
insert x = set . (x :) . toList  

delete :: (Eq a, Show a) => a -> Set a -> Set a
delete x = fromList . L.filter (not . (==x)) . toList 

without :: (Eq a, Show a) => Set a -> a -> Set a
without = flip delete
-- is very useful for typing 
-- X `without` x 

isMemberOf :: (Eq a, Show a) => a -> Set a -> Bool
isMemberOf x = (x `isIn`) . toList

-- 
isNotMemberOf :: (Eq a, Show a) => a -> Set a -> Bool
isNotMemberOf = curry (not . uncurry (isMemberOf)) 

null ::(Eq a, Show a) => Set a -> Bool
null = (== empty) 

size :: Integral i => Set a -> i
size (Set [])       = 0
size s@(Set (x:xs)) = 1 + size (s `without` x) 

isSubsetOf :: Set a -> Set a -> Bool
isSubsetOf (Set xs) (Set ys) = xs `areIn` ys

x != y = not (x == y)

isProperSubsetOf ::(Eq a, Show a) => Set a -> Set a -> Bool
isProperSubsetOf s s' = (s `isSubsetOf` s') && (s != s') 
-- nomes bons?

disjoint :: (Eq a, Show a) => Set a -> Set a -> Bool
disjoint s s' = (inter s s') == empty 

isDisjointWith :: (Eq a, Show a) => Set a -> Set a -> Bool
isDisjointWith = disjoint

pairwiseDisjoint :: Set (Set a) -> Bool
pairwiseDisjoint (Set xs) = conj bs 
	where bs = [x `isDisWithAll` ys| x <- xs]

union :: (Eq a, Show a) => Set a -> Set a -> Set a
union s s' = set $ (toList s) ++ (toList s')

inter :: (Eq a, Show a) => Set a -> Set a -> Set a
inter s s' = set [x | x <- (toList s), x `isMemberOf` s']  
-- escrever com map e filter dos sets!

-- relative complement (set difference)
setminus :: Set a -> Set a -> Set a
setminus = undefined

(\\) = setminus
infixr 5 \\

unions :: Set (Set a) -> Set a
unions = undefined

inters :: Set (Set a) -> Set a
inters = undefined

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct = undefined

disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion = undefined

filter :: (a -> Bool) -> Set a -> Set a
filter p (Set xs) = Set (L.filter p xs)  

-- que nome estranho
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition = undefined

map :: (Eq a, Show a, Eq b, Show b) => (a -> b) -> Set a -> Set b
map f = set . (L.map f) . toList 

