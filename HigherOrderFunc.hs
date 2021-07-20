import Prelude hiding (filter)

-- Question 1

{-
Notice that generator for `y` and `z` depend on `x` and `y` respectively, so
that we ensure the relation x <= y <= z and avoid duplication.

There is some problem with the sample output in the handout (Sorry).
But you can make all the generator [1..n] so that they don't dependent on each
other (just like the sample output in the handout).
-}

pythagoreans :: Int -> [(Int, Int, Int)]
pythagoreans n =
  [(x, y, z) |
    x <- [1..n],
    y <- [x..n],
    z <- [y..n],
    x ^ 2 + y ^ 2 == z ^ 2
  ]

-- Question 2.

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n - 1], isPerfect x]
  where isPerfect x =
          sum [factor | factor <- [1 .. x - 1], x `mod` factor == 0] == x

-- Question 3.

{-
You may want to understand what this "dot" is doing.
Try `:type (.)` in your ghci.

But doing it the old fashion way is also OK.
-}

filtMap :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtMap f p = map f . filter p 

-- Question 4.

-- ditto

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs  = map (uncurry f) . zip xs 

-- Question 5.

length :: [a] -> Int
length = foldr (const (+1)) 0 

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

{-
Notice here we can pattern matching inside a lambda expression,
which makes the implementation a lot simpler.
-}

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(x, y) (xs, ys) -> (x : xs, y : ys)) ([], [])

{-
`flip` swap the order of a binary operator so that `:` is able to fit into the
parameter of foldl.
foldl turns the `:` operator left associative, so that foldl apply `:` to the
elements of the list from the beginning to the end, which is the exact reverse
order of how a list is originally constructed.
-}

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

-- Question 6.

{-
Now this may be a bit tricky for you guys, so proceed if you are interested.

The idea is, for either of the problem, we go through the list in an order,
but we want to apply the function `f` in a reverse order. So that we want to
"remember" all the elements we meet when we are traversing the list. And
finally pull the trigger of application when the initial value `z` is (finally)
applied.

To accomplish this, we fold the list to yield a function of `b -> b`, says
when you provide me the initial value, I would chain all the application of
`f` together, and give you the final result.

And then we can implement the argument function for the inner fold. When we
encounter a value in `[a]`, we put the computation involving the value
in front of the computation of the traversed value.
-}

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z xs = foldl (\acc x -> acc . f x) id xs z

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = foldr (\x acc -> acc . (`f` x)) id xs z

-- Question 7
-- Note that the lambda expression of "map" can also be transformed

f1 :: [Int] -> Bool
f1 = (> 100) . sum . map ((+1) . (*2)) . filter even

-- Question 8

{-
To "compose" the final result, `a -> (b -> d)`
let's start with `f`, whose type is `a -> (b -> c)`.
Then we work on constructing a function of type `(b -> c) -> (b -> d)`
And the observation is, composing `g :: c -> d` and `b -> c` gets us the result.
So here comes the answer.

If you are interested, try this:
```
compose3 :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
```
-}

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 g f = (g .) . f

-- Question 9

{-
Because we use the parameter of `doSomething` in several spot in the function
definition, so that we are not able to construct proper "cut point" for `splitAt`
EASILY (I believe it's still feasible), after we factor the parameter `xs` out.

So that is one of the situations where you want to use `$` to simplify your code,
(removing nested parenthesis), and `.` may not be appropriate.
-}

-- Question 10

{-
The intuition is:
filter p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

And then we do an eta conversion:
filter p    = foldr (\x acc -> if p x then x : acc else acc) []

And we want to move the lambda expression to the last parameter, so we
make use of the `flip` utility to swap the order of parameters of foldr:
-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = flip foldr [] $ \x acc ->
    if p x then x : acc else acc


data Tree a
    = Leaf
    | Node (Tree a) a (Tree a)

-- Question 11

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf
treeMap f (Node l a r) = Node (treeMap f l) (f a) (treeMap f r)

-- Question 12

{-
The idea is fairly simple, we want to traverse all the elements in tree, who
reside in the `Node` variant.

For the `Leaf` case, we just returning the initial value, for sure.
For the `Node` case, since we are simulating the `foldr`, we want to traverse
the right branch first, and then apply the accumulate function `f`, and then
traverse the left branch.

And what we actually do, is `fold` the right branch with initial value,
we get a result of type `b`, and we apply the accumulate function, getting a
new result of type `b`. Then we TAKE THIS RESULT AS THE INITIAL VALUE of the
folding process of left branch, getting the final result.
-}

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ z Leaf = z
foldTree f z (Node l a r) = foldTree f (f a (foldTree f z r)) l

data Expr
    = Val Int
    | Add Expr Expr
    | Mul Expr Expr

foldExpr :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr v _ _ (Val n) = v n
foldExpr v a m (Add x y) = a (foldExpr v a m x) (foldExpr v a m y)
foldExpr v a m (Mul x y) = m (foldExpr v a m x) (foldExpr v a m y)

-- Question 13. straightforward.

-- Guess what this "return" is doing.

collect :: Expr -> [Int]
collect = foldExpr return (++) (++)
