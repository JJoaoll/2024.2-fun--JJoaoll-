module Arvore where 

data Arvore = Folha (Int) | No (Arvore) (Arvore)
  deriving (Show, Eq)
