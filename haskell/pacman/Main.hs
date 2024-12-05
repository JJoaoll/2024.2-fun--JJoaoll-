module Main where 

import Map 
import Sprites

main :: IO () 
main = putChar 'a'

wrap :: a -> IO a
wrap = pure
--
-- moves :: Coordinate -> IO Coordinate
-- moves (Coordinate (x, y)) = 
--   do 
--     c <- getChar
--     wrap  
--       case (x, y, c) of
--         (,O,'w') 
--         ()
--
--     -- TODO: limitar movimentos
--
--
testMap = createMap 10 0
--
-- -- testPlay :: Pacman -> Map a -> IO ()
-- -- testPlay (Pacman (Coordinate (x, y))) map =
-- --   do 
-- --     undefined
--
--
--
