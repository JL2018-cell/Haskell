import Data.List (isPrefixOf)
import Data.Char (isSpace)
import Control.Monad (void)

newtype Parser a = P {unP :: String -> [(a, String)]}

parse :: Parser a -> String -> [(a, String)]
parse = unP

char :: Char -> Parser Char
char c = P $ \s -> case s of
  [] -> []
  c' : s' -> if c == c'
    then [(c, s')]
    else []

string :: String -> Parser String
string s = P $ \input ->
  if s `isPrefixOf` input
    then [(s, drop (length s) input)]
    else []

(+++) :: Parser a -> Parser a -> Parser a
P l +++ P r = P $ \s ->
  case l s of
    [] -> r s
    success -> success

-- These two functions are both methods that are necessary to implement the
-- `Monad` typeclass. `fmap` is a method for `Functor`, which is a "super class"
-- or `Monad`, and `>>=` is a method for `Monad` itself.
-- So for the sake of clarity we implement the full monad hierarchy here.

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  -- The resulting parser runs the original parser first, and map every result
  -- of that parser by the function
  fmap f (P p) = P $ \s ->
    map (\(a, s') -> (f a, s')) (p s)
  -- or with Control.Arrow.first
  -- fmap f (P p) = P $ map (first f) . p

instance Applicative Parser where
  -- pure :: a -> Parser a
  -- It's equivalent to `return`
  pure a = P $ \s -> [(a, s)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- You don't have to understand this function,
  -- we just skip the implementation with `(>>=)`
  pf <*> pa = pf >>= \f -> pa >>= \a -> return (f a)

instance Monad Parser where
  -- return :: a -> Parser a
  -- SURPRISE! `return` is actually a method for monad too.
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  P p >>= f = P $ \s ->
    -- Read the explanation bottom-up.
    -- finally, we concatenate all the results
    concat $
      -- second, for each of the result `(a, s')`, we generate a new parser
      -- with `f a`, and then obtain the result for each of the new parser
      map (\(a, s') -> parse (f a) s')
        -- first, we run the first parser
        (p s)
    -- So essentially, what we have done is that we run the first parser,
    -- obtain its result, produce the second parser (with `f`), and run
    -- the second parser with the rest of the string. But since there may be
    -- multiple parses after each parser is ran, so we must traverse and combine
    -- all the possibilities together.

    -- This could be simplified if we are to exploit more monad operations
    -- on list (Yes, "List" is also monad), which does the `map` and `concat`
    -- at the same time.
    -- P p >>= f = P $ \s ->
    --   p s >>= (\(a, s') -> parse (f a) s')
    --
    -- which could still be simplified, but it's too fancy to be shown here.

failure :: Parser a
failure = P $ const []

item :: Parser Char
item = P $ \s ->
  case s of
    [] -> []
    (c : s') -> [(c, s')]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
    then return c
    else failure

-- The idea is `item` is able to detect whether there is still character left
-- in the input string, so that we are able to mark difference state of the
-- parser with different boolean value.
eof :: Parser ()
eof = ((item >>= const (return False)) +++ return True) >>= \b ->
  if b then return () else failure

{-
The `do` notation is merely a syntactic sugar for `(>>=)` (and some
other more trivial monad operator). So the "sequencing" behavior is from the
sequential calls of `(>>=)`.
So that if any of the step in the sequence is a "parse failure", we consult
what happen when (>>=) is called on a failure parse. And `(>>=)` relies on
the parse "result" to produce new parsers to continue the chaining, for a
failure parse, there would be no result, so no new parser is generated, so
`(>>=)` will propagate the failure down the line.
Therefore all the subsequent parsers will act trivially and a final failure
will be produced.

Understanding the behavior of `(>>=)` is essential for understanding "do"
notation.
-}

some :: Parser a -> Parser [a]
some p = do
  -- We run the parser once, obtain its result.
  -- If the parser fail here, since we are expecting ONE or more parses, the
  -- whole parser fail.
  a <- p

  -- And then we recurse down to `some`, two things may happen:
  -- 1. parser success with the result list, we prepend the first result to
  --    the result of the recursive call.
  -- 2. parser fail, which is resulted in the first line `a <- p` and we know
  --    that there is no more parse could happen. So we provide a fallback
  --    option that denote we only find exactly one parse.
  fmap (a :) (some p) +++ return [a]

  -- Alternatively, previous line could factor out the `fmap` by:
  -- fmap (a :) (some p +++ return [])

many :: Parser a -> Parser [a]
many p = some p +++ return []

-- Control.Monad.void :: Functor f => f a -> f ()
-- in this case:
-- Control.Monad.void :: Parser [Char] -> Parser ()
skipSpaces :: Parser ()
skipSpaces = void $ many $ satisfy isSpace

token :: Parser a -> Parser a
token p = do
  skipSpaces
  a <- p
  skipSpaces
  return a

-- Or if we exploit the (<*) and (*>) operator for Monad (Applicative)
-- (<*) operator discard the result of its right-hand-side, (*>) operator
-- discard the result of its left-hand-side, but they still carry out
-- the parsing action.

-- (*>) :: Applicative f => f b -> f a -> f a
-- (<*) :: Applicative f => f a -> f b -> f a
-- in this case:
-- (*>) :: Parser () -> Parser a -> Parser a
-- (<*) :: Parser a -> Parser () -> Parser a
token' :: Parser a -> Parser a
token' p = skipSpaces *> p <* skipSpaces

separate :: Parser b -> Parser a -> Parser [a]
separate sep a = do
  -- Run the parser for `a` for the first time
  first <- a
  -- For rest, we run multiple (sep >>= const a), which run the parser for
  -- separator first, and then run `a` again, and obtain the list (maybe null)
  -- as the rest of the parse
  rest <- many (sep >>= const a)
  -- or as we did in `token'`
  -- rest <- many (sep *> a)

  return (first : rest)
