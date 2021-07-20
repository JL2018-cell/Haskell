module Tutorial5 where

import           Control.Monad
import           Data.List
import           Text.Printf

{-

Reading a line from a file could be side-effectful, since
it violates the most important property of functional programming:

It doesn't guarantee to return the same result whenever we call it.
Because it relies on the state of the execution environment of the
environment (the working directory, the filesystem and so on). Such
functions are "bad" because it potentially makes it hard for us to
reason about the behavior of the code by just looking at the code.

And moreover, reading and file does involve mutating the state of
the environemnt. Opening the file descriptor, reading line from a
file descriptor, and closing the file all involves the state mutation
of the environment.

-}

-- Assuming the input integer is positive

nim :: Int -> IO ()
-- initialize the state of the board and who is going
nim x = do
  let board = [x, x - 1 .. 1]
  printBoard board
  playNim board P1

data Player = P1 | P2

switchP :: Player -> Player
switchP P1 = P2
switchP P2 = P1

instance Show Player where
  show P1 = "Player 1"
  show P2 = "Player 2"

-- main logic of the game
-- parameter "board" is the state of the board in a sequence of
-- integer, denoting the number of stars left on certain row
-- parameter "p" is the player who take the action
playNim :: [Int] -> Player -> IO ()
playNim board p = if all (== 0) board
  then putStrLn $ printf "%s wins!" (show $ switchP p)
  else do
    putStrLn "" >> print p
    board' <- boardAction board
    printBoard board'
    playNim board' (switchP p)
 where
  -- for the reason of simplicity (or laziness of mine), we omit verifying
  -- the validity of user inputs, and let the program crash if the input
  -- is invalid
  boardAction :: [Int] -> IO [Int]
  boardAction b = do
    putStr "Enter a row number: "
    row <- readLn
    putStr "Star to remove: "
    n <- readLn
    return $ modifyList (max 0 . subtract n) (row - 1) b

  -- modify the element of a list at specific position
  modifyList :: (a -> a) -> Int -> [a] -> [a]
  modifyList f 0 (h : t) = f h : t
  modifyList f n (h : t) = h : modifyList f (n - 1) t
  modifyList _ _ []      = []

printBoard :: [Int] -> IO ()
printBoard board = putStrLn "" >> printBoard' board >> putStrLn ""
 where
  printBoard' :: [Int] -> IO ()
  -- This is the main logic of printing the board
  --
  -- `zip b [1 ..]` is the trick we used to tag every row with its index, and
  -- now we have a list [(Int, Int)], the number of the stars on the row, with
  -- the index of the row, these are all information we need to print the board.
  --
  -- And then we need to print it, and it should be a fairly easy task with
  -- recursion. But here we introduce a utility in Control.Monad
  -- forM_ :: Monad m => [a] -> (a -> m b) -> m ()
  -- which is the abstraction of traversing the list, and apply an "action"
  -- (in this case, IO) to every element in the list, specified by the second
  -- parameter (the `a -> m b`).
  printBoard' b = forM_ (zip b [1 :: Int ..])
    $ \(ns, idx) -> putStrLn $ printf "%d: %s" idx (showStars ns)

  -- intersperse :: a -> [a] -> [a]
  -- insersperse insert the element between every two elements in the list
  showStars :: Int -> String
  showStars = intersperse ' ' . (`replicate` '*')


join2 :: Monad m => m (m a) -> m a
join2 = (>>= id)

-- alternatively...
-- join mma = mma >>= id

join4 :: Monad m => m (m b) -> m b
join4 mma = mma >>= id

-- or with do notation...
-- join mma = do
--    ma <- mma
--    ma

join3 :: Monad m => m (m b) -> m b
join3 mma = do
     ma <- mma
     ma
--join3 mma = mma >>= (\ma->ma)


fmapM :: Monad m => (a -> b) -> m a -> m b
fmapM f = (>>= return . f)
-- My method: fmapM f xs = xs >>= return . f
-- alternatively...
-- fmapM f ma = ma >>= return . f

-- or fully expand the (.)
-- fmapM f ma = ma >>= \a -> return (f a)

{-

The issue with `Functor`, we can think of it from two perspectives (they are
essentially the same):

1. `fmap` only lets us talk about the "value" inside the `Functor`, but not the
   `Functor` object itself. Looking at the signature of `fmap`

   `fmap :: Functor f => (a -> b) -> f a -> f b`

   The capability `fmap` is that, it let us mutate the "value" inside the `Functor`
   with the function, but leave anything else about the `Functor` object `f a`
   intact. For example, we can only use `fmap` to change the individual elements
   in a list but not the length or anything else about the list.

2. `Functor` doesn't provide us a way to "squash" nested functor into one.
   We could implement a function of signature:

   `Functor f => f a -> (a -> f b) -> f (f b)`

   which is only a special case of `fmap` if you look at it carefully. We can
   only continue stacking functor inside another functor, but we are never able
   to squash the nested functor like `join` of `Monad` does.
   (`Functor f => f (f a) -> f a` is impossible to implement)
   While (>>=) itself requires this ability. So another way to looking at
   (>>=) is that, actually `ma >>= f = join (fmap f ma)`, which is very
   interesting.

-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = xs >>= \x -> if p x then [x] else []

returnMaybe :: a -> Maybe a
returnMaybe = Just

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma f = case ma of
  Nothing -> Nothing
  Just a  -> f a

returnEither :: a -> Either e a
returnEither = Right

bindEither :: Either e a -> (a -> Either e b) -> Either e b
bindEither ea f = case ea of
  Left  e -> Left e
  Right a -> f a


