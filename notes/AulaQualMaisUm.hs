module AulaQualMaisUm where 

import AulaQual
import qualified Prelude as P

data Maybe a where 
	Nothing :: Maybe a 
	Just    :: a -> Maybe a 
	deriving(P.Eq, P.Show)

map :: (a -> b) -> Maybe a -> Maybe b 
map _  Nothing = Nothing
map f (Just x) = Just (f x)

-- leis de functor importam nas listas
-- map id      = id 
-- map (f o g) = map f o map g

--typeclass Functor (f : * -> *)
--	fmap : (a -> b) -> (f a -> f b)
	-- fmap id = id
	-- fmap (f o g) = fmap f o fmap g


-- u -> _





