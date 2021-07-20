import Prelude hiding (concat, map, filter, sum, product, length, reverse, zip, drop, (++))

fac n | n == 0 = 1
      | n > 0  = n * fac (n-1)

length :: [a] -> Int
length (_:xs) = 1 + length xs
length []     = 0

-- reverse [1,2,3]
-- > [3,2,1]

reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = reverse xs ++ [x]

-- zip [1,2,3,4,5] "Hello"
-- > [(1,'H'), (2,'e'), (3,'l') ...]

-- zip [1,2,3] "Hello"
-- > [(1,'H'), (2,'e'), (3,'l')]

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x,y) : zip xs ys
zip _        _        = []

-- drop 2 [1,2,3,4,5]
-- > [3,4,5]

drop :: Int -> [a] -> [a]
drop n []     = []
drop 0 xs     = xs
drop n (x:xs) = drop (n-1) xs

-- [1,2,3] ++ [4,5]
-- > [1,2,3,4,5]

-- [1,1,3] ++ [5,5]
-- > [1,1,3,5,5]


(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
xs     ++ [] = xs
(x:xs) ++ ys = x : (xs ++ ys)


