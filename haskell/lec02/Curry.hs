module Curry where

import Prelude hiding (curry, uncurry)

cplus :: (Int, Int) -> Int
cplus (x,y) = x + y

plus :: Int -> Int -> Int
plus x y = x + y

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f a b = f (a, b) 

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b  
-- uncurry f w = f (outl w) (outr w) 

outl :: (a,b) -> a
outl (a,b) = a 

outr :: (a,b) -> b
outr (a,b) = b
