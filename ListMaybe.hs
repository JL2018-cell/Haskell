{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module ListMaybe
  ( LM(..)
  , ML(..)
  ) where
import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.List

--Problem:
--What if the case: Nothing?
--Result cannot be displayed.

newtype LM a = LM { getLM :: [Maybe a] }
  deriving (Functor, Show)
-- LM :: [Maybe a] -> LM a
-- getLM :: LM a -> [Maybe a]

instance Applicative LM where
  pure = return
  (<*>) = liftM2 ($)
-- return :: Monad m => a -> m a
 -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- | Problem 4.1
instance Monad LM where
  return :: a -> LM a
  return x = LM [Just x]

  (>>=) :: LM a -> (a -> LM b) -> LM b
  (LM [Nothing]) >>= f = LM [Nothing]
  (LM [Just x]) >>= f = f x
  (LM (Nothing:xs)) >>= f = LM ([Nothing] ++ (getLM (LM xs >>= f)))
  (LM ((Just x):xs)) >>= f = LM ((getLM (f x)) ++ (getLM (LM xs >>= f)))
--p>>=k = LM (concat [getLM (k x)| x<-(catMaybes (getLM p))])

--Examples of functions

lmf :: Int->LM Int
lmf x = LM [Just (x+1)]

lmf2 :: [a]->LM Int
lmf2 xs = LM [Just (length xs)]

lmf3 :: [a]->LM [a]
lmf3 xs = LM [Just (xs++[head xs])]


newtype ML a = ML { getML :: Maybe [a] }
  deriving (Functor, Show)

-- ML :: Maybe [a] -> ML a
-- getML :: ML a -> Maybe [a]

instance Applicative ML where
  pure = return
  (<*>) = liftM2 ($)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- pure :: Applicative f => a -> f a

-- | Problem 4.2
instance Monad ML where
  return :: a -> ML a
  return x = ML (Just [x])

  (>>=) :: ML a -> (a -> ML b) -> ML b
  (ML Nothing) >>= f = ML Nothing
  (ML (Just [x])) >>= f = f x
  (ML (Just (x:xs))) >>= f = if null ys then
                               ML Nothing
                             else
                               ML (Just ys)
    where ys = concat ((maybeToList (getML (f x))) ++ (maybeToList (getML ((ML (Just (xs)) >>= f)))))

--(maybeToList (getML (f x))) ++ (maybeToList ((ML (Just xs)) >>= f))

mlf :: Int->ML Int
mlf x = ML (Just [(x+1)])

mlf1 :: [Int]->ML [Int]
mlf1 xs = ML (Just [(map (1+) xs)])

mlf2 :: Char->ML String
mlf2 x = ML (Just [x:""])

mlf3 :: a->ML a
mlf3 x = ML (Just (replicate 3 x))

--mlf4 :: a->ML Int
--mlf4 xs = ML (Just [foldr (\x y->y+1) 0 xs])

