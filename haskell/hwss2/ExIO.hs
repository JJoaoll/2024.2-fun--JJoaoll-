module ExIO where
import System.IO (hSetEcho, stdin)

import qualified Data.Char as C
import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    , interact
    , (>>)
    , (>>=)
    )

-- read through the whole module first, to get an idea
-- of what's required and to decide where to start
-- 
-- Use getChar e putChar
--

-- getLine :: IO String
-- getLine = 
  -- do  
    -- hSetEcho stdin False
    -- c <- getChar 
    -- hSetEcho stdin True
    -- putChar c 
    -- case c of 
      -- '\n' -> pure "" 
      -- _    -> pure (c : getLine) 




getInt :: IO Int
getInt = C.digitToInt <$> getChar

getSafeInt :: IO (Maybe Int)
getSafeInt = undefined

-- sequencing: first do f ignoring its result, then do g and keep its result
infixl 1 >>

-- Dá pra ler como "E depois"
(>>) :: IO a -> IO b -> IO b
ax >> ay =
  do
    ax
    ay

(<<) :: IO b -> IO a -> IO b
(<<) = flip (>>)


-- pauses till the user presses any normal key
pause :: IO ()
pause =
    do
        hSetEcho stdin False
        getChar
        hSetEcho stdin True
        -- pure ()

skip :: IO ()
skip = undefined

newline :: IO ()
newline =
  do
    putChar '\n'


-- falha
-- gambiarra :: [IO a] -> ()
-- gambiarra [] = ()

-- define it as a foldr
putStr :: String -> IO ()
putStr ""     = pure ()
putStr (c:cs) =
  do putChar c
     putStr cs

-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize iob x =
  do
    -- putChar '\n'
    b <- iob x
    putChar '\n'
    pure b

putStrLn :: String -> IO ()
putStrLn = lnize putStr

putCharLn :: Char -> IO ()
putCharLn = lnize putChar

-- reads the entire user input as a single string, transforms it, and prints it
interact :: (String -> String) -> IO ()
interact = undefined

perlineize :: (String -> String) -> (String -> String)
perlineize f = unlines . map f . lines

interactPerLine :: (String -> String) -> IO ()
interactPerLine = interact . perlineize

when :: Bool -> IO () -> IO ()
when False _ = pure ()
when True  f = do f

unless :: Bool -> IO () -> IO ()
unless = when . not

guard :: Bool -> IO ()
guard = undefined

forever :: IO a -> IO b
forever f =
  do
    f
    forever f

-- transforms the action given to an equivalent one that has no result
void :: IO a -> IO ()
void action =
    do
        _ <- action
        pure ()

-- Kleisli compositions
infixr 1 >=>, <=<

-- diagrammatic order
(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
f >=> g = \a ->
    do
        b <- f a
        g b


-- traditional order
-- comparison of types:
-- (.)   :: (b ->    c) -> (a ->    b) -> a ->    c
-- (<=<) :: (b -> IO c) -> (a -> IO b) -> a -> IO c
(<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) = flip (>=>)

-- Bind
infixl 1 >>=

(>>=) :: IO a -> (a -> IO b) -> IO b
ax >>= f = undefined

infixl 4 $>, <$

-- make an action that has the side effects of the action on the left
-- but with result the value on the right
($>) :: IO a -> b -> IO b
ax $> y = undefined

-- vice-versa
(<$) :: a -> IO b -> IO a
x <$ ioy = undefined

ap :: IO (a -> b) -> IO a -> IO b
af `ap` ax = undefined

filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO = undefined

iomap :: (a -> b) -> IO a -> IO b
iomap f actA =
    do
       -- que sintaxe é essa?? 
       -- f <$> actA
       a <- actA
       pure $ f a

-- dá pra melhorar
mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO _ []     = pure []
mapIO f (x:xs) =
  do
    f x
    mapIO f xs

zipWithIO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithIO = undefined

zipWithIO_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithIO_ = undefined

sequenceIO :: [IO a] -> IO [a]
sequenceIO []     = pure []
sequenceIO (f:fs) =
    do
      x  <- f
      let fs' = sequenceIO fs
      error "falta terminar"

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ = undefined

replicateIO :: Integral i => i -> IO a -> IO [a]
replicateIO = undefined

replicateIO_ :: Integral i => i -> IO a -> IO ()
replicateIO_ = undefined

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO = undefined

forIO_ :: [a] -> (a -> IO b) -> IO ()
forIO_ = undefined

joinIO :: IO (IO a) -> IO a
joinIO = undefined

foldlIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlIO = undefined

foldlIO_ :: (b -> a -> IO b) -> b -> [a] -> IO ()
foldlIO_ = undefined


