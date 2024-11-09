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

delete :: a -> Set a -> Set a
delete = undefined

member :: a -> Set a -> Bool
member = undefined

notMember :: Set a -> Bool
notMember = undefined

null :: Set a -> Bool
null = undefined 

size :: Integral i => Set a -> i
size = undefined

isSubsetOf :: Set a -> Set a -> Bool
isSubsetOf = undefined

isProperSubsetOf :: Set a -> Set a -> Bool
isProperSubsetOf = undefined

disjoint :: Set a -> Set a -> Bool
disjoint = undefined

pairwiseDisjoint :: Set (Set a) -> Bool
pairwiseDisjoint = undefined

union :: Set a -> Set a -> Set a
union = undefined

inter :: Set a -> Set a -> Set a
inter = undefined

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
filter = undefined

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition = undefined

map :: (a -> b) -> Set a -> Set b
map = undefined

