module ExFunctor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const


instance Funktor [] where
    fmap :: (a -> b) -> [a] -> [b] 
    fmap = map

instance Funktor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b 
    fmap _ Nothing  = Nothing 
    fmap f (Just x) = Just (f x)

-- what about Either?
instance Funktor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b 
  fmap _ (Left e)  = Left e
  fmap f (Right x) = Right (f x)

-- instance Funktor (Either _ v) where 
-- Osu (How)

-- what about pairs?
instance Funktor (pairs a) where
  fmap :: (a -> b) -> Either e a -> Either e b 
  fmap _ (Left e)  = Left e
  fmap f (Right x) = Right (f x)



-- what about functions?

-- what about Trees?

-- what about IO? 

-- ...define Functor instances of as many * -> * things as you can think of!

