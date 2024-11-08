data Unit = O 

instance (Show Unit) where
  show o = "()"

data Empty 

f :: Num a => () -> a
f = \x -> 42

haha :: Empty
haha = error "haha"

test :: Empty -> String 
test _ = "hehe" 
 
test2 :: Empty -> String 

data Box a = Box a

o :: Empty -> Nat 
so :: Box Empty -> Nat 
sso :: Box (Box Empty) -> Nat
ssso :: Box (Box (Box Empty))-> Nat

-- Algumas perguntas:
-- site haskell em loop 
-- gambiarra lean 
-- so par corretude?

data M a where
  N : M a
  J : a -> M a 

data True  = True 
data False = False

data Result a where 
  Okay  : a      -> Result a
  Error : String -> Result a 

data ThenElse a b where 
  Then : a -> ThenElse a b
  Else : b -> ThenElse a b 




