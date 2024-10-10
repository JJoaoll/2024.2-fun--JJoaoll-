module List where

data List a = Empty | Cons a (List a) 
    deriving(Show, Eq)

hd:: List a -> Maybe a
hd Empty        = Nothing    
hd (Cons x xs)  = Just x  

tl :: List a -> List a
tl Empty        = Empty
tl (Cons x xs)  = xs 

cat :: List a -> List a -> List a 
cat Empty ys       = ys 
cat (Cons x xs) ys = Cons x (cat xs ys)

filterr :: (a -> Bool) -> List a -> List a
filterr _ Empty           = Empty 
filterr judge (Cons x xs) = if judge x 
                            then Cons x recFilter 
                            else recFilter
               where recFilter = filterr judge xs

(+++) :: [a] -> [a] -> [a]
[] +++ ys     = ys 
(x:xs) +++ ys = x:(xs +++ ys)

len :: [a] -> Int
len []       = 0
len (x : xs) = 1 + len xs 

rev :: [a] -> [a]
rev [] = [] 
rev (x : xs) = rev xs ++ [x]

app :: [a] -> a -> [a]
app xs x = xs ++ [x]

atLeastTwo :: [a] -> Bool
atLeastTwo (x:y:ls) = True 
atLeastTwo _ = False
  {-  case xs of 
      [] -> 
          False 
      (y:ys) ->
          True -}

firstTwo :: [a] -> (a,a) 
firstTwo (x:y:_) = (x,y)
firstTwo _ = error "nao tem 2 primeiros"
