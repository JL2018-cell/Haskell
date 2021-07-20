

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module Transform
   ( Transform(..)
   , next
   , runTransform
   , evalTransform
   , Tree(..)
   , tFoldl
   , tToListWith
   ) where

import Control.Monad

newtype Transform a b = Transform { getTransform :: (b, a -> a) }
  deriving Functor
-- getTransform :: Transform a b -> (b, a -> a)
-- Transform :: (b, a -> a) -> Transform a b

instance Applicative (Transform a) where
   pure = return
   (<*>) = liftM2 ($)

-- | Problem 2
instance Monad (Transform a) where
   return :: b -> Transform a b
   return x = Transform (x,id)

   (>>=) :: Transform a b -> (b -> Transform a c) -> Transform a c
   p>>=k = Transform ((fst (getTransform (k (fst (getTransform p))))),
           (snd (getTransform p)) . (snd (getTransform (k (fst (getTransform p))))))

next :: (a -> a) -> Transform a ()
next f = Transform ((),f)

evalTransform :: Transform a b -> b
evalTransform = fst . getTransform

runTransform :: Transform a b -> a -> a
runTransform = snd . getTransform

countedFibonacci :: Int -> Transform Int Int
countedFibonacci 0 = return 0
countedFibonacci 1 = return 1
countedFibonacci n = do
   a <- countedFibonacci (n - 1)
   next (+1)
   b <- countedFibonacci (n - 2)
   return $ a + b

data Tree a = Leaf | Branch (Tree a) a (Tree a)
t = Branch (Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)) 4 (Branch (Branch Leaf 5 Leaf) 6 Leaf)
t2 = Branch (Branch (Branch Leaf 1 ((Branch (Branch Leaf 12 Leaf) 10 (Branch Leaf 13 Leaf)))) 2 (Branch Leaf 3 Leaf)) 4 (Branch (Branch Leaf 5 Leaf) 6 Leaf)

-- | Problem 3
tFoldl :: (b -> a -> b) -> b -> Tree a -> Transform [a] b
tFoldl f b Leaf = return b
tFoldl f b (Branch l x r) = do
  left <- tFoldl f b l -- left :: b
  next ([x]++) -- x :: a, [a]->[a]
  mid <- return (f b x)
  right <- tFoldl f b r -- right :: b
  return mid

tToListWith :: (b -> a -> b) -> Tree a -> Transform b [a]
tToListWith f Leaf = return []
tToListWith f (Branch l x r) = do
  right <- (tToListWith f r)
  next (`f` x) --x :: a from Tree
  mid <- return x -- return original element in Tree.
  left <- (tToListWith f l)
  return (left++[mid]++right)


