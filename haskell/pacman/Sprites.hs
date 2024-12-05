module Sprites where 

import Map 
import Nat

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






