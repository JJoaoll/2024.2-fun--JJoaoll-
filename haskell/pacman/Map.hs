module Map where 

import Nat
import ExList  hiding (replicate, map)
import Prelude ( Eq (..), Show (..), (.)
               , IO (..), pure, return, putStrLn
               , replicate
               , ($), undefined 
               ) 

data Map a where 
  Map :: [[a]] -> Map a
  deriving (Eq, Show)

data Coordinate where
  Coordinate :: (Nat, Nat) -> Coordinate 
  deriving (Eq, Show)

-- TODO: generalizar
createMap :: Nat -> a -> Map a
createMap num x = Map $ replicate (fromNat num) (replicate (fromNat num) x)
 
replaceIn :: a -> Coordinate -> Map a -> Map a 
replaceIn _ _ (Map []) = Map [] 
replaceIn v (Coordinate (O, y)) (Map (vs : yss))   =
  Map (replaceWith y vs v : yss)
replaceIn v (Coordinate (S n, y)) (Map (vs : vss)) = 
  let (Map vss') = replaceIn v (Coordinate (n, y)) (Map vss) 
  in   Map(vs : vss')

wrap :: a -> IO a 
wrap = pure 

printMap :: Show a => Map a -> IO () 
printMap (Map vss) = undefined 
  -- putStrLn $ concatWith '\n' (map map show vss)




  



