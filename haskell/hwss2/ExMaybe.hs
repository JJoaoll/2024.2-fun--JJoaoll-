module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes (m : ms) =
  case m of 
    Just _ -> 
    _      ->

fromJust :: Maybe a -> a
fromJust = undefined

fromMaybe :: a -> Maybe a -> a
fromMaybe = undefined

isJust :: Maybe a -> Bool
isJust (Just _) = True 
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust 

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe = undefined

justMap :: (a -> Maybe b) -> [a] -> [b]
justMap = undefined

maybe :: b -> (a -> b) -> Maybe a -> b
maybe = undefined

maybeToList :: Maybe a -> [a]
maybeToList = undefined

listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith = undefined

