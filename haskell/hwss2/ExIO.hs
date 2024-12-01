{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
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



getLine :: IO String
getLine =
  do
    c <- getChar
    case c of
      '\n' -> pure ""
      _    ->
        do
          s <- getLine
          pure (c : s)



{-
getLine :: IO String
getLine = 
  do 
    c <- getChar 
    cs <- case c of 
            '\n' -> pure "" 
            _    -> getLine
    case c of 
      '\n' -> pure      cs 
      _    -> pure (c : cs)
-}

-- se receber um scroll, ele retorna um cast de todo o scroll
scrollCaster :: [IO a] -> IO [a]
scrollCaster []               = pure []
scrollCaster (spell : scroll) =
  do
    x  <- spell
    xs <- scrollCaster scroll
    pure $ x : xs


doWhile :: IO a -> (a -> Bool) -> IO ()
doWhile spell p =
  do
    a <- spell
    if p a then doWhile spell p
         else pure ()


getInt :: IO Int
-- getInt = C.digitToInt <$> getChar
getInt =
  do
    str <- getLine
    let num = read str
    return num

getSafeInt :: IO (Maybe Int)
getSafeInt =
  do
    str <- getLine
    if hasDigits str
    then return $ Just $ read $ digits str
    else return Nothing

  where digits    = filter C.isDigit
        hasDigits = any C.isDigit 


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
    -- hSetEcho stdin True
    pure ()

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
putStr cs = foldl (<<) (pure ()) (map putChar cs)
-- foldl            :: (b -> a -> b) -> b -> [a] -> b 
-- (<<)             :: IO b -> IO a -> IO b
-- pure ()          :: IO () 
-- (map putChar cs) :: [IO ()]


-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize spell x =
  do
    -- putChar '\n'
    b <- spell x
    putChar '\n'
    pure b

putStrLn :: String -> IO ()
putStrLn = lnize putStr

putCharLn :: Char -> IO ()
putCharLn = lnize putChar

-- reads the entire user input as a single string, transforms it, and prints it
-- interact :: (String -> String) -> IO ()
-- interact f =
  -- do 
    -- str <- getline
    -- putStr $ f str
    -- -- pure ()
-- 
perlineize :: (String -> String) -> (String -> String)
perlineize f = unlines . map f . lines

interactPerLine :: (String -> String) -> IO ()
interactPerLine = undefined --interact . perlineize

when :: Bool -> IO () -> IO ()
when False _ = pure ()
when True  f = do f

unless :: Bool -> IO () -> IO ()
unless = when . not

guard :: Bool -> IO ()
guard b = undefined

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

-- w de writer ou r de reader
filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO _ []       = pure []
filterIO w (x : xs) =
  do
    b   <- w x
    xs' <- filterIO w xs
    if b then pure (x : xs')
         else pure xs'


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
zipWithIO _ [] _          = return []
zipWithIO _ _ []          = return []
zipWithIO w (x:xs) (y:ys) =
  do
    z  <- x `w` y
    zs <- zipWithIO w xs ys
    return (z : zs)


zipWithIO_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithIO_ w xs ys =
  do
    _ <- zipWithIO w xs ys
    return ()


sequenceIO :: [IO a] -> IO [a]
sequenceIO = scrollCaster

{-
sequenceIO []     = pure []
sequenceIO (f:fs) =
    do
      x  <- f
      let fs' = sequenceIO fs
      error "falta terminar"
-}

-- oq significam esses "_"?
-- eh pq não retorna algo?
sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ scroll =
  do
    scrollCaster scroll
    pure ()


replicateIO :: Integral i => i -> IO a -> IO [a]
replicateIO num spell =
  if num <= 0 then pure []
  else
    do
      x  <- spell
      xs <- replicateIO (num - 1) spell
      pure (x : xs)


replicateIO_ :: Integral i => i -> IO a -> IO ()
replicateIO_ num spell = do
    _ <- replicateIO num spell
    pure ()


forIO :: [a] -> (a -> IO b) -> IO [b]
forIO [] _       = return []
forIO (x : xs) w =
  do
    y  <- w x
    ys <- forIO xs w
    return (y : ys)


forIO_ :: [a] -> (a -> IO b) -> IO ()
forIO_ xs action =
  do
    _ <- forIO xs action
    pure ()


joinIO :: IO (IO a) -> IO a
joinIO = undefined

foldlIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlIO = undefined

foldlIO_ :: (b -> a -> IO b) -> b -> [a] -> IO ()
foldlIO_ = undefined


