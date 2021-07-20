--Question 1
--Share a box of n mooncakes with m firends.

solve :: Int -> Int -> [Int]
solve n m = reverse (aDD (d m (n `div` m)) (n `mod` m) (n `mod` m) (n `div` m))
  where d 0 num = []
        d count num = num : d (count-1) num
        aDD xs 0 rm rl = xs
        aDD (x:xs) num2 rm rl = (x+1) : aDD xs (num2-1) rm rl

--Question 2
--Returns a Boolean indicating whether the string is in the list or not.
ismember :: (Eq a)=>a -> [a] -> Bool
ismember elm [] = False
ismember elm (x:xs) = (elm==x) || ismember elm xs

--Question 3
--Test if a number is lucky or not.
checkHappyDigits :: [Int]->Int->Int
checkHappyDigits [] elm = 0
checkHappyDigits (x:xs) elm = (judge elm x 0) + (checkHappyDigits xs elm)
  where judge :: Int -> Int -> Int -> Int
        judge e1 e2 e3 | e2<1 && e3>e1 = 0
                       | e2<1 && e3<=e1 = 1
                       | e2>=1 && (((e2 `mod` 10) == 7) || ((e2 `mod` 10) == 4)) = judge e1 (e2 `div` 10) (e3+1)
                       | e2>=1 && (((e2 `mod` 10) /= 7) && ((e2 `mod` 10) /= 4)) = judge e1 (e2 `div` 10) e3
                       | otherwise = 0

--Question 4
--Number of ways to construct a binary search tree.
f2 :: Integer->Integer
f2 n = (product [(n+2)..(2*n)]) `div` (product [1..n])
numBST :: Integer->Integer
numBST n = (f2 n) `mod` 1000000007

--Question 5
--find such integer point (with integer coordinates) of the plane, that belongs to exactly k drawn squares.
desc1 :: Ord a => [a] -> [a]
desc1 [] = []
desc1 (x:xs) = desc1 larger ++ [x] ++ desc1 smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

pointIn :: [Int] -> Int -> [Int]
pointIn xs n | (length xs) >= n && n==1 = [z2,0]
             | (length xs) >= n && n>1 = pointIn (take n ys) (n-1)
             | otherwise = [-1]
  where (z2:z2s) = desc1 xs
        (y:ys) = desc1 xs

--Question 6
--A list is defined to be twin paired if its even-valued elements (if any) are in ascending order and its odd-valued elements (if any) are in descending order.
isTwinPaired :: [Int] -> Bool
isTwinPaired [] = True
isTwinPaired [x] = True
isTwinPaired xs = (evenJ [ e | e<-xs, e `mod` 2 ==0]) && (oddJ [ o1 | o1<-xs, o1 `mod` 2 /=0])
  where evenJ :: [Int]->Bool
        evenJ ys = and [a<=b | (a,b) <- zip ys (tail ys)]
        oddJ :: [Int]->Bool
        oddJ zs = and [a>=b | (a,b) <- zip zs (tail zs)]

--Question 7
--Computes the number of ways to place N knights on an N × N chessboard such that none of knights are conflicted with each other.
pmu :: [a] -> [[a]]
pmu xs = dP xs []
  where
    dP [] _ = [[]]
    dP [y] ys = map (\x->[y]++x) (dP ys [])
    dP (y : ys) zs = dP [y] (ys ++ zs) ++ dP ys (y : zs)

q'' :: Int -> [[Int]]
q'' n = filter (\x ->check(zip[1..n] x)) choose
  where choose = pmu [1..n]
        confict (x1,y1) (x2,y2) = ((y1==y2)||(x1==x2)|| ((abs(x1-x2))==abs(y1-y2))||(abs(x1-x2)<=2 && abs(y1-y2)<=1)||(abs(x1-x2)<=1 && abs(y1-y2)<=2))
        check [] = True
        check(q:qs)= (not $ (any (\x ->confict q x) qs )) && check(qs)

chess :: Int -> Int
chess n = length (q'' n)







