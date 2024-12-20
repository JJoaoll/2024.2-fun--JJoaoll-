{-# LANGUAGE KindSignatures #-}

module FA where

import Prelude hiding
    ( Functor(..)
    , fmap
    , (<$>) , (<$) , ($>) , (<&>)
    , unzip
    , Applicative(..)
    , pure
    , (<*>) , (<*) , (*>) , (<**>)
    , liftA , liftA2 , liftA3
    )

import Util
import qualified Data.Functor as F
import qualified Control.Applicative as A


----------------------------------------------------------------
-- Functor
----------------------------------------------------------------

class Functor (f :: * -> *) where

  fmap :: (a -> b) -> (f a -> f b)

  (<$) :: b -> f a -> f b
  y <$ fx = fmap (const y) fx

  {- LAWS

     fmap id = id
     fmap (f . g) = fmap f . fmap g

   -}

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

-- Oq justifica esse nome?? done
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip = pairing (fst <$>) (snd <$>)
-- unzip fw = (outl <$> fw, outr <$> fw) 

void :: Functor f => f a -> f ()
void = (() <$)

-- syntactic associativity and precedence
infixl 4 <$>, $>, <$
infixl 1 <&>

----  Instances  -----------------------------------------------

-- List
instance Functor [] where
    fmap f (x : xs) = f x : fmap f xs
    fmap _ _        = []

-- Maybe
instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

-- (α ×)
instance Functor ((,) a) where
    fmap f (x, y) = (x, f y)

-- (α +)
instance Functor (Either a) where
    fmap f (Left  v) = Left v
    fmap f (Right e) = Right (f e)

-- (r →)
instance Functor ((->) r) where
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap = (.)

-- IO
instance Functor IO where
    fmap f ax = do f <$> ax;


----------------------------------------------------------------
-- Applicative
----------------------------------------------------------------

class Functor f => Applicative (f :: * -> *) where

  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  {- LAWS
   
     pure f <*> v      = f <$> v
     
     pure id <*> v     = v 

     pure f <*> pure x = pure (f x)

     u <*> pure y      = ?

     u <*> (v <*> w)   = ((.) <$> u)) <*> v) <*> w
                        

   -}

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f = (pure f <*>)

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 g fx fy = pure g <*> fx <*> fy

  -- (<*>) :: f (a -> b) -> f a -> f b
-- sequence actions, discarding the value of the first argument
(*>) :: Applicative f => f a -> f b -> f b
(*>) = flip (<*)

-- sequence actions, discarding the value of the second argument
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const

-- A variant of (<*>) with the types of the arguments reversed.
-- It differs from flip (<*>) in that the effects are resolved
-- in the order the arguments are presented.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = undefined

when :: Applicative f => Bool -> f () -> f ()
when True  = id
when False = const $ pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless = when .  not 

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL = undefined
-- sequenceAL []         = pure []
-- sequenceAL (fx : fxs) = lx 
--    where lx = (:[]) `liftA` fx


-- syntactic associativity and precedence
infixl 4 <*>, *>, <*, <**>

----  Instances  -----------------------------------------------

-- Maybe
instance Applicative Maybe where
    pure = undefined
    (<*>) = undefined

-- Lists with ambiguous computational aspect (non-determinism):
-- Create an isomorphic copy of the type List a, called Ambiguous a
newtype Ambiguous a = Ambiguous [a]

-- show ambiguous lists like this: ?[1,2,3]
instance Show a => Show (Ambiguous a) where
  show (Ambiguous xs) = "?" <> show xs

instance Functor Ambiguous where
    fmap = undefined

instance Applicative Ambiguous where
    pure = undefined
    (<*>) = undefined

-- Lists with temporal computational aspect (sequencial):
-- Create an isomorphic copy of the type List a, called Temporal a
-- (the isomorphism is given by the constructor/wrapper Temporal : List a -> Temporal a)
newtype Temporal a = Temporal [a]

-- show temporal lists like this: →[1,2,3]
instance Show a => Show (Temporal a) where
  show (Temporal xs) = "→" <> show xs

instance Functor Temporal where
    fmap = undefined

instance Applicative Temporal where
    pure = undefined
    (<*>) = undefined

-- IO
instance Applicative IO where
    pure = undefined
    (<*>) = undefined

-- (m ×)
instance Monoid m => Applicative ((,) m) where
    pure = undefined
    (<*>) = undefined

-- (s +)
instance Semigroup s => Applicative (Either s) where
    pure = undefined
    (<*>) = undefined

-- (r →)
instance Applicative ((->) r) where
    pure = undefined
    (<*>) = undefined

