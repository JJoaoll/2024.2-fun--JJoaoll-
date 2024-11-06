module ExArith where

data ArEx = Atom Integer
          | Plus ArEx ArEx
          | Times ArEx ArEx
          | Neg ArEx
  deriving (Eq, Show)



-- pretty printer
pretty :: ArEx -> String
pretty (Atom n) = show n
pretty (Plus l r) =   "(" ++ (pretty l) ++ " + " ++ (pretty r) ++ ")"
pretty (Times l r) =   "(" ++ (pretty l) ++ " * " ++ (pretty r) ++ ")"
pretty (Neg ex) = "-" ++ "(-" ++ pretty ex ++ ")"

pA :: ArEx -> String 
pA (Atom x)       = " " ++ show x ++ " "
pA ex@(Plus l r)  = lastEx ++ "\n" ++ take (length totalS) (repeat '-')  ++ " (+) \n" ++ (pretty ex) 
  where lastEx = pA l ++ pA r 
        totalS = pretty l ++ pretty r
pA ex@(Times l r) = lastEx ++ "\n" ++ take (length totalS) (repeat '-')  ++ " (*) \n" ++ (pretty ex)  
 where lastEx = pA l ++ pA r
       totalS = pretty l ++ pretty r 
pA ex@(Neg t) = lastEx ++ "\n" ++ take (length totalS) (repeat '-')  ++ " (-) \n" ++ (pretty ex)
 where lastEx = pA t 
       totalS = pretty t

prettier = putStrLn . pA

-- example expressions
ex1 = (Atom 23) `Plus` (Atom 2)
ex2 = (Atom 7) `Times` ((Atom 7) `Plus` ((Atom 2) `Times` (Atom 8)))
ex3 = Times ex1 ex2
ex4 = Neg $ ex3 `Plus` ex1
ex5 = (Neg ex1) `Times` (Neg ex4)

-- eval evaluates an expression and returns its value
eval :: ArEx -> Integer
eval (Atom t)     = t 
eval (Plus t t')  = eval t + eval t' 
eval (Times t t') = eval t * eval t'
eval (Neg t)      = negate (eval t)


-- step should make only 1 step of calculation on a given ArEx
step :: ArEx -> ArEx
step (Atom t)     = Atom t 
step (Plus t t')  = Atom (eval t + eval t')
step (Times t t') = Atom () 
step (Neg t)      = Atom (negate (eval t))

  

