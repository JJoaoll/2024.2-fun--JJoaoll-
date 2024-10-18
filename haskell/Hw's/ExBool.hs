module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True 

instance Show Bool where
    show True = "T"
    show _    = "F"

instance Enum Bool where
    toEnum x = if x >= 0  
               then True 
               else False 

    fromEnum True  = 1 
    fromEnum False = -1

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && True = True 
_ && _       = False 

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
False || False = False 
_ || _         = True 
infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) b = not . (&&) b 

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) b = not . (||) b 

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) b1 b2 = not (b1 && b2 || b1 \|/ b2)

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False 
not _    = True 

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a1 _ = a1 
ifThenElse _ _ a2    = a2

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
True ==> False = False 
_ ==> _        = True 

infixr 1 ==>

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) = flip (==>)

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
b1 <=> b2 = (b1 ==> b2) && (b1 <== b2)  
-- (<=>) b = not . (<=/=>) b 

infixr 1 <=>


