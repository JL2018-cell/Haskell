{-# LANGUAGE LambdaCase #-}

module Regex
  ( Regex
  , regexP
  , parseRegex
  , matcher
  ) where

import Combinators
import Parser
import Text.Printf

import Prelude hiding (fail)

import Control.Applicative hiding (optional)
import Control.Monad       hiding (fail)
import Control.Monad.State hiding (fail)

import Data.Char
import Data.Functor
import Data.List
import Data.Monoid

{-|

Probably a more straightforward definition would be:

@
data Regex
  = RChar Char
  | RAny
  | RStar Regex
  | RPlus Regex
  | ROptional Regex
  | ROr Regex Regex
  | RConcat Regex Regex
@

Probably a better definition would be to separate each level of precedence like:

@
data Regex2
  = RChar Char
  | RAny
  | RStar Regex
  | RPlus Regex
  | ROptional Regex

data Regex1 = Concat Regex2 Regex2

data Regex = Or Regex1 Regex1
@

My personal motivation is to only avoid the parsing ambiguity of "abcd", so I
separate the concatenation case from the other.

-}

data Regex'
  = RChar Char
  | RAny
  | RStar Regex
  | RPlus Regex
  | ROptional Regex
  | ROr Regex Regex

infixr 5 :::, /++/

data Regex
  = RNil
  | Regex' ::: Regex


instance Show Regex' where
  {-
  type ShowS :: String -> String

  showsPrec :: Int -> Regex -> ShowS

  This is an alternative method to implement in show, which accepts the
  "precedence" as an additional argument.

  It doesn't matter you don't know it, you should implement your own `showsPrec`
  like `showPrec :: Int -> Regex -> String`

  The idea is to recurse into the structure of `Regex`, and pass the precedence
  of each expressions to their sub-expressions.
  With this invariant fixed, the precedence in each case is the precedence of
  the "outer" expression. So you can compare the precedence of current
  expression to the outer expression to see if the parenthesis should be added.

  I make a heavy use of the library utilities here, look them up if you
  are interested. You can view all the `.` below as `++` in a usual
  implementation. But without them, it should also be easy to implement with
  the idea above.
  -}

  showsPrec p r = case r of
    RChar c -> showChar c
    RAny -> showChar '.'
    RStar r' -> unaryShows p r' '*'
    RPlus r' -> unaryShows p r' '+'
    ROptional r' -> unaryShows p r' '?'
    ROr r1 r2 -> showParen (p > 1) $
      showsPrec 1 r1 . showChar '|' . showsPrec 1 r2
    where
      unaryShows :: Int -> Regex -> Char -> ShowS
      unaryShows _ (RChar c ::: RNil) op = showParen (p >= 10) $
        showChar c . showChar op
      unaryShows p r op = showParen (p >= 10) $
        showsPrec 10 r . showChar op

instance Show Regex where
  showsPrec p r = case r of
    RNil        -> id
    r' ::: RNil -> showsPrec p r'
    r' ::: r2   -> showParen (p > 5) $ showsPrec 5 r' . showsPrec 5 r2

-- Utility to "concatenate" two `Regex`es.
(/++/) :: Regex -> Regex -> Regex
(/++/) RNil r = r
(r' ::: r1) /++/ r2 = r' ::: r1 /++/ r2

-- Data.Functor.($>) :: Functor f => f a -> b -> f b
starP :: Parser (Regex -> Regex')
starP = char '*' $> RStar

plusP :: Parser (Regex -> Regex')
plusP = char '+' $> RPlus

optionalP :: Parser (Regex -> Regex')
optionalP = char '?' $> ROptional

postfixOp :: Parser (Regex -> Regex')
postfixOp = choose [starP, plusP, optionalP]

-- Basic characters are either (satisfy isAlphanum), or (char '.')
charP :: Parser Regex'
charP = RChar <$> satisfy isAlphaNum <|> char '.' $> RAny

-- Regular expression inside parenthesis
--
-- Control.Applicative.(*>) :: f a -> f b -> f b
-- Control.Applicative.(<*) :: f a -> f b -> f a
parenP :: Parser Regex
parenP = char '(' *> (($ RNil) <$> orP) <* char ')'

-- By the precedence, there must be character or parenthesis
-- inside postfix expressions
postfixP :: Parser Regex'
postfixP = do
  r <- (::: RNil) <$> charP <|> parenP
  f <- postfixOp
  return $ f r

-- Main case for parsing concatenation,
-- This `Regex -> Regex` is a trick for a "general" behavior of concatenation,
-- without it, it should be similar to implement with the usual concat operator.
concatP :: Parser (Regex -> Regex)
concatP
  -- The main idea is that:
  -- a concatenation of regular expression is a sub-regex with equal or higher
  -- precedence than postfix expressions (postfix, character, parenthesis)
  -- followed by a concatenation of regular expression
  = atom <|> liftA2 (.) atom concatP
  where
    atom :: Parser (Regex -> Regex)
    atom = ((:::) <$> (charP <|> postfixP)) <|> ((/++/) <$> parenP)

-- Main case for parsing "|" operator.
-- similar idea from above, the both sides of an "|" expression should be of
-- higher-precedence than concat.
-- And I use a similar recursive approach to the approach above.
orP :: Parser (Regex -> Regex)
orP = do
  r <- concatP
  o <- optional $ char '|'
  case o of
    Nothing -> return r
    Just _  -> (((::: RNil) . ROr (r RNil)) .) <$> orP

-- And we put everything together, and adding an restriction of `eof`.
regexP :: Parser Regex
regexP = ($ RNil) <$> orP <* eof

-- Simple utility function that are not specified in the assignment.
parseRegex :: String -> Maybe Regex
parseRegex
  = getAlt
  . mconcat
  . fmap (Alt . Just . fst)
  . filter (null . snd)
  . runParser regexP


{-
The idea is to implement a matcher that match from the first character, and
enumerate all the suffixes of the input string.

Really glad to see some of you pulled off this minimal definition, good job.
The idea is to skip through characters an arbitrary amount of times, and let
the monadic behavior of parser handle all the logic.
-}
matcher :: Regex -> Parser String
matcher r = many next >> matcher' r
-- Alternatively:
-- matcher = (many next >>) . matcher'

{-
A matcher that match (or parse) from the first character of input
should be straightforward recursion for both functions, with the use of
appropriate combinators from the template.

Control.Applicative.liftA2
  :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

(<$>) is equivalent to fmap

Data.Functor.(<$>) :: Functor => (a -> b) -> f a -> f b
-}
matcher' :: Regex -> Parser String
matcher' = \case
  RNil -> string ""
  r' ::: r -> liftA2 (++) (matcher'' r') (matcher' r)

matcher'' :: Regex' -> Parser String
matcher'' = \case
  RChar c -> return <$> char c
  RAny    -> return <$> next
  RStar r -> concat <$> many (matcher' r)
  RPlus r -> concat <$> some (matcher' r)
  ROptional r -> return "" <|> matcher' r
  ROr r1 r2 -> matcher' r1 <|> matcher' r2
