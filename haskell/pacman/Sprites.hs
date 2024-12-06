module Sprites where 

import System.IO (hSetEcho, stdin)
import Map 
import Nat hiding (B, R)

-- data State where 
--   Dead      :: State
--   Alive     :: State 
--   PoweredUp :: State -- Pacman tunado
--   Chasing   :: State -- Fantasma perseguindo
--   Frighted  :: State -- Fantasama assustado
--   Scatter   :: State -- Fantasma voltando pra posicao inicial
--   Eaten     :: State -- Fantasma comido
--   Patroling :: State -- Fantasma andando random'icamente

-- ghost tem chasing e normal 
data GhostStatus where 
  Patroling :: GhostStatus  -- Random moves
  Chasing   :: GhostStatus  -- Chasing moves
  Frighted  :: GhostStatus  -- Comível
  Scatter   :: GhostStatus  -- Saindo do meio pra pos padrão
  Eaten     :: GhostStatus  -- Voltando pro meio 

data Pacman where 
  Pacman :: { pacmanPos  :: Coordinate
            }            -> Pacman

data Ghost where 
  Ghost :: { ghostPos    :: Coordinate
           , ghostStatus :: GhostStatus 
           }             -> Ghost

data Sprite where
  Player :: Pacman -> Sprite
  AI     :: Ghost  -> Sprite

data Direction where
  T :: Direction -- Top
  L :: Direction -- Left
  R :: Direction -- Right
  B :: Direction -- Bot

-- x -> vertical   (descendo)
-- y -> horizontal (indo pra direita)

move :: Coordinate -> Direction -> Coordinate
move (Coordinate (x, y)) dir  =
  case dir of
    T -> Coordinate (px, y) 
    L -> Coordinate (x, py) 
    R -> Coordinate (x, S y) 
    B -> Coordinate (S x, y) 

  where px     = pred x 
        py     = pred y
        pred n = case n of 
                   O   -> O
                   S n -> n

getMove :: IO Direction 
getMove = 
  do
    hSetEcho stdin False
    c <- getChar
    case c of
      'h' -> wrap L
      'j' -> wrap B
      'k' -> wrap T
      'l' -> wrap R
      _   -> getMove

-- IO (Coordinate, Map a, (a,a))

testPlay :: Show a => Coordinate -> Map a -> (a, a) -> IO ()
testPlay c@(Coordinate (x, y)) map@(Map vss) (sprite, bg) = 
  do
    printMap map
    m <- getMove
    let new_c   = move c m
    let new_map = replaceIn sprite new_c (replaceIn bg c map)
    testPlay new_c new_map (sprite, bg)
  

-- tryToMove :: Show a => Pacman -> Map a -> IO (Map a)
-- tryToMove (Pacman (Coordinate (x, y))) m = 
--   do
--     c <- getChar
--     wrap m 



