{-# LANGUAGE LambdaCase #-}

module Combinators
  ( lookAhead
  , optional
  , choose
  , followedBy
  , manyUntil) where

import Parser

import Prelude hiding (fail)
import Control.Monad.State hiding (fail)
import Control.Applicative hiding (optional)
import Control.Monad hiding (fail)

import Control.Arrow

import Data.Monoid
import Data.Functor

-- The syntax of "LambdaCase" extension
-- \case ... is equivalent to \s -> case s of ...
lookAhead :: Parser Char
lookAhead = parser $ \case
  [] -> []
  c : cs -> [(c, cs)]

{-
Advance: exploiting the Parser in the template is an instance of `State` monad

lookAhead = get >>= \case
  [] -> failure
  c : _ -> return c
-}

-- Control.Arrow.first :: (a -> b) -> (a, c) -> (b, c)
-- (<$>) is equivalent to `fmap` (and therefore `map`)
optional :: Parser a -> Parser (Maybe a)
optional p = parser $ \s -> case runParser p s of
    [] -> [(Nothing, s)]
    rs -> first Just <$> rs


choose :: [Parser a] -> Parser a
choose = foldr (<|>) failure

{-
Advance: exploiting the Parser is an instance of `Alternative`

choose = getAlt . mconcat . fmap Alt
-}


-- A more general lookAhead function, generalized to lookAhead a parser
lookAheadP_ :: Parser a -> Parser ()
lookAheadP_ p = parser $ \s -> case runParser p s of
    [] -> []
    _  -> [((), s)]

-- And the implementation of main function is straightforward
followedBy :: Parser a -> Parser b -> Parser a
followedBy pa pb = do
    a <- pa
    lookAheadP_ pb
    return a

-- A "straight" implementation:

-- Control.Applicative.liftA2
--   :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
--
-- Data.Functor.($>) :: f a -> b -> f b
manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil pa pb = pb $> [] <|> liftA2 (:) pa (manyUntil pa pb)

{-
And I believe you can use `many` directly:

Control.Applicative.(<*) :: Applicative f => f a -> f b -> f a

manyUntil pa pb = many pa <* pb
-}