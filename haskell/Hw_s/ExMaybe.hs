module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes = undefined

fromJust :: Maybe a -> a
fromJust = undefined

fromMaybe :: a -> Maybe a -> a
fromMaybe = undefined

isJust :: Maybe a -> Bool
isJust = undefined

isNothing :: Maybe a -> Bool
isNothing = undefined

listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = undefined

maybe :: b -> (a -> b) -> Maybe a -> b
maybe = undefined

maybeToList :: Maybe a -> [a]
maybeToList = undefined

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith = undefined

