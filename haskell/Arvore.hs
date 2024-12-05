module Arvore where 

import Prelude hiding (min, divides, (++), concat)
import Nat hiding (n, max)
import ExList 
--import Data.List 

-- Uma arvore nao estritamente binaria
-- que carrega valores nos nos, 
-- mas todos os valores sao do msm tipo

-- Solucao de Lima:
data Tree a where 
  Nil  :: Tree a 
  Node :: a -> Tree a -> Tree a -> Tree a
  deriving Eq 

rev :: Tree a -> Tree a
rev Nil = Nil 
rev (Node val l r) = (Node val (rev r) (rev l))


instance (Show a) => (Show (Tree a)) where
  show Nil           = "Nil"
  show (Node x t t') = "Node " ++ show x ++ " (" ++ show t ++ ", " ++ show t' ++ ")" 

data Path = L | X | R
  deriving (Eq, Show)

data Result e v where
  Error :: e -> Result e v
  Ok    :: v -> Result e v
  deriving (Eq)

instance (Show e, Show v) => (Show (Result e v)) where
  show (Ok v)    = "Ok ("    ++ show v ++ ")"
  show (Error e) = "Error (" ++ show e ++ ")"
 
data FindError where
  EmptyPath :: FindError
  deriving (Eq, Show)
 
find :: Show a => [Path] -> Tree a -> Result FindError a
find [] _ = Error EmptyPath 

-- devo lidar com erros aqui?
-- da pra melhorar
showFloor :: Show a => Nat -> Tree a -> String 
showFloor _  Nil               = ""
showFloor O (Node x _ _)      = show x
showFloor (S n) (Node _ t t') = showFloor n t ++ " " ++ showFloor n t' 

prettyAux :: Show a =>  Tree a -> String
prettyAux t = concat [showFloor n t ++ "\n" | n <- [0 .. h]] 
  where h = heigth t

--pretty :: Show a => Tree a -> IO
pretty t = putStrLn (prettyAux t)

--- tree examples
t0 = Nil
t1 = Node 3  Nil Nil
t2 = Node 4  t1 t0 
t3 = Node 15 t2 t1
t4 = Node 20 t3 t2
t5 = Node 8  t4 t3
t6 = Node 7  t5 t4
---

heigth :: Tree a -> Nat
heigth Nil            = O
heigth (Node _ t1 t2) = S $ max t1' t2'
  where t1' = heigth t1
        t2' = heigth t2



{-

instance (Eq a) => (Eq (Arvore a))     where 
  Folha x == Folha x'           = (x == x')
  No1 x t == No1 x' t'          = (x == x') && (t == t') 
  No2 x t1 t2 == No2 x' t1' t2' = (x == x') && (t1 == t1') && (t2 == t2') 

altura :: Arvore a -> Nat 
altura (Folha _)  = 1 
altura (No1 _ t)    = 1 + (altura t)
altura (No2 a t t') = 1 + (max (altura t) (altura t'))  

isIn :: (Eq a, Show a) => a -> Arvore a -> Bool 
x `isIn` Folha v    = x == v 
x `isIn` No1 v t    = x == v || x `isIn` t 
x `isIn` No2 v t t' = x == v || x `isIn` t || x `isIn` t'

isNotIn :: (Eq a, Show a) => a -> Arvore a -> Bool 
isNotIn x = not . (isIn x)

-- O default pro No1 eh ter seu unico filho como L
-- Find paths to a number in a tree
find :: (Eq a, Show a) => a -> Arvore a -> [[Path]]
find x (No1 v t) 
  | x == v     = [[X]] 
  | x `isIn` t = []--[ ([L] ++ find x t)] 
  | otherwise  = []
find x (Folha v) = if x == v then [[X]] else [] 
find x (No2 v t t') 
  | x == v = [[X]] 
  | (x `isIn` t)    && (x `isIn` t')    = [] 
  | (x `isIn` t)    && (x `isNotIn` t') = [] 
  | (x `isNotIn` t) && (x `isIn` t')    = []

-}





