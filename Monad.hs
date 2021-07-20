module Lecture9 where

data Option a = None | Some a deriving Show 

instance Functor Option
instance Applicative Option

instance Monad Option where
  -- return :: a -> Option a
  return x = Some x
  -- (>>=) :: Option a -> (a -> Option b) -> Option b
  (Some x) >>= f = f x
  None >>= f     = None


safediv :: Int -> Int -> Option Int
safediv x 0 = None
safediv x y = return (div x y)

-- (x / y) + (y / x)

p x y = do r1 <- safediv x y
           r2 <- safediv y x
           return (r1 + r2)
