module ExEither where

-- Do not alter this import!
import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

-- booleanismo.. 

isLeft :: Either a b -> Bool
isLeft (Left _) = True 
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft 

lefts :: [Either a b] -> [a]
lefts [] = [] 


rights :: [Either a b] -> [b]
rights = undefined

fromLeft :: a -> Either a b -> a
fromLeft = undefined

fromRight :: b -> Either a b -> b
fromRight = undefined

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = undefined

either :: (a -> c) -> (b -> c) -> Either a b -> c
either = undefined

