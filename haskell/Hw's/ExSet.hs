module ExSet
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

import qualified Data.List as L

data Set a = Set [a]

-- CAUTION: you may need to add constraints to your types and instances!

instance Eq (Set a) where
    xs == ys  = undefined

instance Show (Set a) where
    show xs = "{ please define show }"

-- smart constructor
set :: [a] -> Set a
set = fromList

empty :: Set a -> Bool
empty = undefined

singleton :: a -> Set a
singleton = undefined

fromList :: [a] -> Set a
fromList = undefined

toList :: Set a -> [a]
toList = undefined

powerSet :: Set a -> Set (Set a)
powerSet = undefined

insert :: a -> Set a -> Set a
insert = undefined

delete :: a -> Set a -> Set a
delete = undefined

member :: a -> Set a -> Bool
member = undefined

notMember :: Set a -> Bool
notMember = undefined

null :: Set a -> Bool
null = 

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

