import Prelude hiding (concat, replicate, (!!), elem)

{-
Basic use of recursion.
-}

concat :: [[a]] -> [a]
concat [] = []
concat (xs : xss) = xs ++ concat xss

-- tail recursive version
concat' :: [[a]] -> [a]
concat' = concat'' []  -- Why can we partially apply the function concat''?
  where
    concat'' :: [a] -> [[a]] -> [a]
    concat'' result [] = result
    concat'' result (xs : xss) = concat'' (result ++ xs) xss

replicate :: Int -> a -> [a]
replicate n x
    | n < 0  = error "invalid amount"
    | n == 0 = []
    | otherwise = x : replicate (n - 1) x

-- tail recursive version
replicate' :: Int -> a -> [a]
replicate' n x = replicate'' [] n
  where replicate'' result 0 = result
        replicate'' result n = replicate'' (x : result) (n - 1)

-- this is already tail recursive
(!!) :: [a] -> Int -> a
[] !! _ = error "invalid index"
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x : xs) = a == x && elem a xs

-- tail recursive version
elem' :: Eq a => a -> [a] -> Bool
elem' a = elem'' False
  where elem'' result [] = result
        elem'' result (x : xs) = elem'' (result && a == x) xs


{-
We implement fib recursively and iteratively.

Note that the efficiency of second (iterative) implementation is significantly
better than the first (recursive)one, since the first implementation wastes a
lot of computation to figure out the value of a same term of fibonacci number.

e.g.
fib 10 = fib 9 + fib 8
       = (fib 8 + fib 7) + fib 8
       = (fib 7 + fib 6) + fib 7 + (fib 7 + fib 6)

Here it calculates (fib 7) three times already.
-}


fib :: Int -> Int
fib n
  | n < 0 = error "invalid index"
  | n < 2 = n
  | otherwise = fib (n - 1) + fib (n - 2)

fib' :: Int -> Int
fib' = fibRec 0 1
    where
      fibRec x y n
        | n < 0 = error "invalid index"
        | n == 0 = x
        | otherwise = fibRec y (x + y) (n - 1)


listDouble :: [Int] -> [Int]
listDouble [] = []
listDouble (x : xs) = x * 2 : listDouble xs

listDouble' :: [Int] -> [Int]
listDouble' = map (* 2)


zipSum :: [Int] -> [Int] -> [Int]
zipSum [] _ = []
zipSum _ [] = []
zipSum (x : xs) (y : ys) = x + y : zipSum xs ys

zipSum' :: [Int] -> [Int] -> [Int]
zipSum' = zipWith (+)


merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
-- Here we use the "as pattern" for simplicity
-- See https://www.haskell.org/tutorial/patterns.html for detail.
merge l@(x : xs) r@(y : ys)
    | x < y = x : merge xs r
    | otherwise = y : merge l ys



msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort l) (msort r)
      where (l, r) = splitAt (length xs `div` 2) xs
