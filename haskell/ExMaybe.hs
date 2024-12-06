module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes []     = [] 
catMaybes (m:ms) = case m of  
                   Nothing -> ms'
                   Just x  -> x:ms'
  where ms' = catMaybes ms

-- E tem como? 
fromJust :: Maybe a -> a
fromJust (Just x) = x 
fromJust _        = error "Nothing here"

fromMaybe :: a -> Maybe a -> a
fromMaybe e Nothing  = e 
fromMaybe _ (Just x) = x 

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False 

isNothing :: Maybe a -> Bool
isNothing = not . isJust 

safeHead :: [a] -> Maybe a 
safeHead []    = Nothing 
safeHead (x:_) = Just x

listToMaybe :: [a] -> Maybe a
listToMaybe = safeHead 

-- untested
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
-- mapmaybe f = catmaybes . (map f) 

mapMaybe f []       = []
mapMaybe f (x : xs) = 
  case f x of
    Just y -> y : ys
    _      -> ys
  where ys = mapMaybe f xs
  
-- mapmaybe f = catmaybes . (map f) 

-- untested
maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing  = x
maybe _ f (Just y) = f y

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = [] 
maybeToList (Just x) = [x]

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith = undefined  



