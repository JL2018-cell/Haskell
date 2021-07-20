import Prelude hiding (concat,map, filter, sum, product, or, and, foldr, length, reverse)

-- > concat [[1,2],[3]]
-- [1,2,3]

concat :: [[a]] -> [a]
concat xss = [ x | xs <- xss, x <- xs]

-- Hint: use n `mod` x == 0

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes  :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- zip "Hi" [0,1,2]
-- [('H',0),('i',1)]

positions :: Eq a => a -> [a] -> [Int]
positions e xs = [b | (a,b) <- zip xs [0..(length xs - 1)], e==a]

-- map as a list comprehension

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

rmap :: (a -> b) -> [a] -> [b]
rmap f []      = []
rmap f (x:xs)  = f x : rmap f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [ x | x <- xs, p x]

sum :: [Int] -> Int
sum     = foldr (+) 0

product :: [Int] -> Int
product = foldr (*) 1

or :: [Bool] -> Bool
or      = foldr (||) False 

and :: [Bool] -> Bool
and     = foldr (&&) True

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op v []     = v
foldr op v (x:xs) = x `op` (foldr op v xs)

len :: [a] -> Int
len = foldr (\x r -> r+1) 0 

-- The following recursive definition of length can
-- be helpful to understand the foldr version

length :: [a] -> Int
length []      = 0
length (x:xs)  = let r = length xs in r+1

-- foldr :: (a -> b -> b) -> b -> [a] -> b

reverse :: [a] -> [a]
reverse xs = foldr (\x r -> r ++ [x]) [] xs
