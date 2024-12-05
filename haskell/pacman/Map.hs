module Map where 

import System.IO (hFlush, stdout)
import System.Process (callCommand)
import Nat
import ExList  hiding (replicate)
import Prelude ( Eq (..), Show (..), (.)
               , IO (..), pure, return, putStrLn
               , replicate
               , ($), undefined 
               , String (..), Char (..)
               ) 

import qualified Prelude as P

data Map a where 
  Map :: [[a]] -> Map a
  deriving (Eq, Show)

fromMap :: Map a -> [[a]]
fromMap (Map vss) = vss

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

fmap :: (a -> b) -> Map a -> Map b 
fmap f (Map vss) = Map $ (map . map) f vss

-- formatMap :: Map String -> String
formatMap :: Map String -> String 
formatMap = (\str -> ' ' : str) . concatWith ' ' . concatWith "\n" . fromMap

formatStrMap str = concatWith ' ' . concatWith "\n"  

printMap :: Show a => Map a -> IO () 
printMap = putStrLn . formatMap . fmap show 
--formatMap :: Show a  => [[a]] -> String
--formatMap str =  concatWith '\n' ((map . map) P.show str)

testmap = createMap 10 0  

testPlay = undefined
    
    


