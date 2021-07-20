import           System.IO.Unsafe
import           System.Random
import           System.IO
import           Control.Applicative            ( (<$>) )
import           Data.Maybe
import           Data.List
import           Data.Function
import           System.Environment
import           System.Process
import           System.Directory

--property of map
--color: colour blocks, need to rerive
--bonus: number of bonus the player gets.
--win: player wins if win==True, player loses otherwise.
--fun: function, store actions defined by player.
data Mapattr = Mapattr { bonus :: Int
                       , color :: Char
                       , win :: Bool
                       , dir :: [String]
                       , fun :: [String]
                       } deriving (Show,Eq)




--Interpret player input
--Case-sensitive
--Warn player if input is invalid.
--if user input not in the list, then error.
check :: String -> String -> Int
check c pc | cf && r1       = 0
           | cf && (not r1) = err + 1
           | otherwise      = 1
 where
  cmd1 = ["Up", "Down", "Left", "Right"]
  cmd2 =
    [ "Cond{" ++ c ++ "}" ++ "{" ++ x ++ "}" | c <- ["o", "y", "p"], x <- cmd1 ]
  cmd3 = cmd1 ++ cmd2
  cmd4 =
    [ "Loop{" ++ show (n) ++ "}" ++ "{" ++ d1 ++ "," ++ d2 ++ "}"
    | n  <- [1 .. 5]
    , d1 <- cmd3
    , d2 <- cmd3
    ]
  cmd5 = cmd4 ++ cmd3 ++ ["Function"]
  [cm] = take 1 $ words c
  cc   = drop 1 $ words c
  gr   = map (`elem` cmd5) cc
  r1   = and gr
  cf   = pc == cm
  err  = fst . head . filter (not . snd) $ zip [1 ..] gr

{-
check :: String -> String -> Bool
check a b = if a == b then True
                else False 
-}
--load map.txt
--Align every line from txt file.
readcontent :: String -> IO [String]
readcontent fileName = do
  contents <- lines <$> readFile fileName
  let s      = map (words) contents
  let readyb = map (++ " ") $ map (intercalate " ") s
  return readyb --contents 


--Generate map dynamically.
putt :: Int -> a -> [a] -> [a]
putt pos newVal list = take pos list ++ newVal : drop (pos + 1) list

--Generate map dynamically.
put2d :: Int -> Int -> a -> [[a]] -> [[a]]
put2d x y newVal mat = putt y (putt x newVal (mat !! y)) mat

--Calculate furtherest distance player go if he chooses right/left direction.
toRight :: String -> Int -> Int
toRight s c = t
 where
  t = if w >= length s then (length s) - 2 else w
  w = c + toEnd r
  r = drop c s

--Player stops if he encounters colour block, target.
toEnd :: [Char] -> Int
toEnd [] = 0
toEnd (c : c1 : cs) | '-' == c && ' ' == c1 = 2 + toEnd cs
                    | 'b' == c && ' ' == c1 = 2 + toEnd cs
                    | 't' == c && ' ' == c1 = 2 + toEnd cs
                    | 'y' == c && ' ' == c1 = 0
                    | 'o' == c && ' ' == c1 = 0
                    | 'p' == c && ' ' == c1 = 0
                    | otherwise             = -2

--Player wins if he meets the target from left/right.
isWin :: [Char] -> Bool
isWin [] = False
isWin (c : c1 : cs) | '-' == c && ' ' == c1 = False || isWin cs
                    | 'b' == c && ' ' == c1 = False || isWin cs
                    | 't' == c              = True
                    | otherwise             = False


--Calculate number of bonus that the player gets.
toBonus :: String -> Int -> Int -> Int
toBonus s b e =
  foldl (\i c -> if c == 'b' then i + 1 else i) 0 $ take (e - b + 1) $ drop b s

--Calculate position of bonus.
toPosb :: String -> Int -> Int -> Int
toPosb s b e = fromMaybe 0 $ elemIndex 'b' $ take (e - b + 1) $ drop b s

--Calculate furtherest distance player go if he chooses up/down direction.
toRight' :: String -> Int -> Int
toRight' s c = t
 where
  t = if w >= length s then (length s) - 1 else w
  w = c + toEnd' r
  r = drop c s

--Calculate furtherest distance player go if he chooses up/down direction.
toEnd' :: [Char] -> Int
toEnd' [] = 0
toEnd' (c : cs) | '-' == c  = 1 + toEnd' cs
                | 'b' == c  = 1 + toEnd' cs
                | 't' == c  = 1 + toEnd' cs
                | 'y' == c  = 0
                | 'o' == c  = 0
                | 'p' == c  = 0
                | otherwise = -1

--Player wins if he meets the target from up/down.
isWin' :: [Char] -> Bool
isWin' [] = False
isWin' (c : cs) | '-' == c  = False || isWin' cs
                | 'b' == c  = False || isWin' cs
                | 't' == c  = True
                | otherwise = False

--Return type of colour block the player encounters.
checkCol :: [String] -> Int -> Int -> (Bool, Char)
checkCol b x y | (b !! y) !! x == 'y' = (True, 'y')
               | (b !! y) !! x == 'o' = (True, 'o')
               | (b !! y) !! x == 'p' = (True, 'p')
               | otherwise            = (False, ' ')

--Player goes to left, transfer current and new attribute to next new map.
moveLeft :: ([String], Mapattr) -> Int -> Int -> (([String], Mapattr), Bool)
moveLeft b x y
  | t
  = ( ( p3
      , Mapattr { bonus = (bonu + e), color = cr, win = t, dir = di, fun = fc }
      )
    , True
    )
  | (not t) && nb >= w
  = (b, False)
  | otherwise
  = ( ( p3
      , Mapattr { bonus = (bonu + e)
                , color = cr
                , win   = False
                , dir   = di
                , fun   = fc
                }
      )
    , True
    )
 where
  t  = isWin $ ((drop (nb + 2) l) ++ " ")
  l  = reverse $ (fst b) !! y
  nb = fromMaybe 0 $ elemIndex '@' l
  w  = toRight (l ++ " ") (nb + 2)
  e  = toBonus (l ++ " ") (nb + 2) w
  pe = (toPosb l (nb + 2) w) + (nb + 2)
  --p1   = put2d (length l - nb -2) y colo (fst b)
  p1 = if colo /= ' '
    then put2d (length l - nb - 1) y colo (fst b)
    else put2d (length l - nb - 1) y '-' (fst b)
  p2       = put2d (length l - w - 1) y '@' p1
  p3       = if e > 0 then put2d (length l - pe - 1) y '-' p2 else p2
  bonu     = bonus $ snd b
  colo     = color $ snd b
  di       = (dir $ snd b) ++ [colo : "l"]
  fc       = fun $ snd b
  (cb, cr) = checkCol (fst b) (length l - w - 1) y

--Player goes to right, transfer current and new attribute to next new map.
moveRight :: ([String], Mapattr) -> Int -> Int -> (([String], Mapattr), Bool)
moveRight b x y
  | t
  = ( ( p3
      , Mapattr { bonus = (bonu + e)
                , color = cr
                , win   = True
                , dir   = di
                , fun   = fc
                }
      )
    , True
    )
  | (not t) && nb >= w
  = (b, False)
  | otherwise
  = ( ( p3
      , Mapattr { bonus = (bonu + e)
                , color = cr
                , win   = False
                , dir   = di
                , fun   = fc
                }
      )
    , True
    )
 where
  t        = isWin $ drop (nb + 2) l
  l        = (fst b) !! y
  nb       = fromMaybe 0 $ elemIndex '@' l
  w        = toRight l (nb + 2)
  e        = toBonus l (nb + 2) w
  pe       = (toPosb l (nb + 2) w) + (nb + 2)
  p1 = if colo /= ' ' then put2d nb y colo (fst b) else put2d nb y '-' (fst b)
  p2       = put2d w y '@' p1
  p3       = if e > 0 then put2d pe y '-' p2 else p2
  bonu     = bonus $ snd b
  colo     = color $ snd b
  di       = (dir $ snd b) ++ [colo : "r"]
  fc       = fun $ snd b
  (cb, cr) = checkCol (fst b) w y

--Player goes up, transfer current and new attribute to next new map.
moveUp :: ([String], Mapattr) -> Int -> Int -> (([String], Mapattr), Bool)
moveUp b x y
  | t
  = ( ( p3
      , Mapattr { bonus = (bonu + e), color = cr, win = t, dir = di, fun = fc }
      )
    , True
    )
  | (not t) && nb >= w
  = (b, False)
  | otherwise
  = ( ( p3
      , Mapattr { bonus = (bonu + e)
                , color = cr
                , win   = False
                , dir   = di
                , fun   = fc
                }
      )
    , True
    )
 where
  t        = isWin' $ drop (nb + 1) l
  l        = reverse $ map (!! x) (fst b)
  nb       = fromMaybe 0 $ elemIndex '@' l
  w        = toRight' l (nb + 1)
  e        = toBonus l (nb + 1) w
  y1       = (length l) - w - 1
  pe       = (toPosb l (nb + 1) w) + (nb + 1)
  p1 = if colo /= ' ' then put2d x y colo (fst b) else put2d x y '-' (fst b)
  p2       = put2d x y1 '@' p1
  p3       = if e > 0 then put2d x ((length l) - pe - 1) '-' p2 else p2
  bonu     = bonus $ snd b
  colo     = color $ snd b
  di       = (dir $ snd b) ++ [colo : "u"]
  fc       = fun $ snd b
  (cb, cr) = checkCol (fst b) x y1

--Player goes down, transfer current and new attribute to next new map.
moveDown :: ([String], Mapattr) -> Int -> Int -> (([String], Mapattr), Bool)
moveDown b x y
  | t
  = ( ( p3
      , Mapattr { bonus = (bonu + e), color = cr, win = t, dir = di, fun = fc }
      )
    , True
    )
  | (not t) && y0 >= w
  = (b, False)
  | otherwise
  = ( ( p3
      , Mapattr { bonus = (bonu + e)
                , color = cr
                , win   = False
                , dir   = di
                , fun   = fc
                }
      )
    , True
    )
 where
  t        = isWin' $ drop (y0 + 1) l
  l        = map (!! x) (fst b)
  y0       = fromMaybe 0 $ elemIndex '@' l
  w        = toRight' l (y0 + 1)
  e        = toBonus l (y0 + 1) w
  pe       = (toPosb l (y0 + 1) w) + (y0 + 1)
  y1       = w
  p1 = if colo /= ' ' then put2d x y0 colo (fst b) else put2d x y0 '-' (fst b)
  p2       = put2d x y1 '@' p1
  p3       = if e > 0 then put2d x pe '-' p2 else p2
  bonu     = bonus $ snd b
  colo     = color $ snd b
  di       = (dir $ snd b) ++ [colo : "d"]
  fc       = fun $ snd b
  (cb, cr) = checkCol (fst b) x w

--command = Loop
recuLoop :: ([String], Mapattr) -> [String] -> (([String], Mapattr), Bool)
recuLoop b s | s == []   = (b, True)
             | otherwise = b2
 where
  b1 = doMove b ("play " ++ head s)
  b2 = recuLoop (fst b1) (drop 1 s)

--Command = Loop
moveLoop
  :: ([String], Mapattr) -> Int -> Int -> String -> (([String], Mapattr), Bool)
moveLoop b x y s = recuLoop b actp4
 where
  act = words [ if c == '{' || c == '}' || c == ',' then ' ' else c | c <- s ]
  loopnum = read (act !! 1) :: Int
  actp1   = drop 8 s
  actp2   = (take ((length actp1) - 1) actp1) ++ " "
  actp3   = concat (replicate loopnum actp2)
  actp4   = words [ if c == ',' || c == ' ' then ' ' else c | c <- actp3 ]

--Command = Condition
moveCond
  :: ([String], Mapattr) -> Int -> Int -> String -> (([String], Mapattr), Bool)
moveCond b x y s | elem col actcolor = doMove b ("play " ++ (act !! 2))
                 | otherwise         = (b, False)
 where
  actcolor = act !! 1
  act      = words [ if c == '{' || c == '}' then ' ' else c | c <- s ]
  col      = color $ snd b

--Command = Function
moveFunc
  :: ([String], Mapattr)
  -> Int
  -> Int
  -> [String]
  -> (([String], Mapattr), Bool)
moveFunc b x y s | t         = b1
                 | otherwise = bs
 where
  l      = (length acts) + 1
  t      = win $ (snd $ fst b1)
  [act1] = take 1 s
  acts   = drop 1 s
  b1     = doMove b ("play " ++ act1)
  bs     = if l > 1 then moveFunc (fst b1) x y acts else b1


-- command : play Left/Up/Cond{p}{Down}/Loop{2}{Cond{p}{Down},Right}
doMove :: ([String], Mapattr) -> String -> (([String], Mapattr), Bool)
doMove b m | words m !! 1 == "Left"           = moveLeft b x y
           | words m !! 1 == "Right"          = moveRight b x y
           | words m !! 1 == "Up"             = moveUp b x y
           | words m !! 1 == "Down"           = moveDown b x y
           | words m !! 1 == "Function"       = moveFunc b x y (fun $ snd b)
           | isPrefixOf "Cond" (words m !! 1) = moveCond b x y (words m !! 1)
           | isPrefixOf "Loop" (words m !! 1) = moveLoop b x y (words m !! 1)
  where (x, y) = startPos $ fst b

--Show total number of bonus the player gets.
bonusShow :: Int -> Int -> IO ()
bonusShow o n | n > o     = putStrLn ("got " ++ on ++ " bonus!")
              | otherwise = putStrLn " "
 where
  nl =
    [ "first"
    , "second"
    , "third"
    , "fourth"
    , "fifth"
    , "sixth"
    , "seventh"
    , "eighth"
    , "ninth"
    , "tenth"
    , "eleventh"
    , "twelfth"
    , "thirteenth"
    , "fourteenth"
    , "fifteenth"
    , "sixteenth"
    , "seventeenth"
    , "eighteenth"
    , "nineteenth"
    , "twentieth"
    ]
  on = if n < length nl then nl !! (n - 1) else show n


--Display message if player meets the target.
winExit :: Bool -> IO ()
winExit b | b == True = putStrLn "Congratulations! You win the game!"
          | otherwise = putStrLn " "

--Display message if movement false.
outInfo :: Bool -> String -> IO ()
outInfo b s
  | b == False
  = putStr "Sorry, error: cannot move to the "
    >> putStrLn ((words s) !! 1)
    >> putStrLn "Your current board:"
  | otherwise
  = putStrLn "New map:"

unsolvExit :: Bool -> IO ()
unsolvExit b | b == False = putStrLn "Unsolvable and exit!"
             | otherwise  = putStrLn " "

--Show a map is solvable or not.
showCheck :: Bool -> IO ()
showCheck b | b == True = putStrLn "Loaded map is solvable!"
            | otherwise = putStrLn "Loaded map is unsolvable!"


-- substrPos substring string
--replace a substring with another substring.
substrPos :: String -> String -> Int
substrPos xs str = pos
 where
  [p]  = map (\x -> findIndex (isPrefixOf x) $ tails str) [xs]
  ppos = fromMaybe 0 $ p
  pre  = if ppos == 0 then isPrefixOf xs str else True
  pos  = if pre == False then -1 else ppos


--doReplace replacedstring newstring stringtobechanged
--replace a substring with another substring.
doReplace :: String -> String -> String -> String
doReplace s sf sx
  | substrPos s sx < 0
  = sx
  | otherwise
  = take (substrPos s sx) sx ++ sf ++ drop ((substrPos s sx) + (length s)) sx

--Replace all occurrence with input string.
doAllrep :: String -> String -> String -> String
doAllrep s sf sx = ssx
 where
  t1  = substrPos s sx >= 0
  t2  = substrPos s s1 >= 0
  s1  = if t1 then doReplace s sf sx else sx
  ssx = if t2 then doAllrep s sf s1 else s1

--Transpose a list of string.
doSwap :: [String] -> [String]
doSwap b = [ map (!! x) b | x <- [0 .. length (head b) - 1] ]

--A sub-function to check a map is solvable or not.
doCount :: [String] -> Int
doCount l = count
 where
  str   = concat l
  count = length $ filter (== '1') str

--A sub-function to check a map is solvable or not.
doCheck :: ([String], Mapattr) -> ([String], Mapattr)
doCheck b | sc == lc  = (ch13, attr)
          | otherwise = doCheck (ch13, attr)
 where
  sc   = doCount $ fst b
  ch0  = map (doAllrep "b" "-") (fst b)
  ch1  = map (doAllrep "y" "-") ch0
  ch2  = map (doAllrep "o" "-") ch1
  ch3  = map (doAllrep "p" "-") ch2
  ch4  = map (doAllrep "t -" "t 1") ch3
  ch5  = map (doAllrep "- t" "1 t") ch4
  ch6  = map (doAllrep "1 -" "1 1") ch5
  ch7  = map (doAllrep "- 1" "1 1") ch6
  ch8  = doSwap ch7
  ch9  = map (doAllrep "t-" "t1") ch8
  ch10 = map (doAllrep "-t" "1t") ch9
  ch11 = map (doAllrep "1-" "11") ch10
  ch12 = map (doAllrep "-1" "11") ch11
  ch13 = doSwap ch12
  lc   = doCount ch13
  attr = snd b

--to check if the map is solvable or not.
doFind :: ([String], Mapattr) -> ([String], Mapattr)
doFind b =
  (nb, Mapattr { bonus = (bonu), color = colo, win = lf, dir = di, fun = fc })
 where
  lf   = lf1 || lf2 || lf3 || lf4
  nb   = fst (doCheck b)
  f1   = map (substrPos "@ 1") nb
  f2   = map (substrPos "1 @") nb
  nbw  = doSwap nb
  f3   = map (substrPos "@1") nbw
  f4   = map (substrPos "1@") nbw
  lf1  = length (filter (>= 0) f1) > 0
  lf2  = length (filter (>= 0) f2) > 0
  lf3  = length (filter (>= 0) f3) > 0
  lf4  = length (filter (>= 0) f4) > 0
  attr = snd b
  bonu = bonus $ attr
  colo = color $ attr
  di   = dir $ attr
  fc   = fun $ attr



--Find starting point of a map.
startPos :: [String] -> (Int, Int)
startPos xs = (x, y)
 where
  [s] = filter (\n -> elem '@' n) xs
  x   = fromMaybe 0 $ elemIndex '@' s
  y   = fromMaybe 0 $ elemIndex s xs

--Find ending point of a map.
endPos :: [String] -> (Int, Int)
endPos xs = (x, y)
 where
  [s] = filter (\n -> elem 't' n) xs
  x   = fromMaybe 0 $ elemIndex 't' s
  y   = fromMaybe 0 $ elemIndex s xs

--Find the property of particular point on the map.
toeq :: [String] -> Int -> Int -> Char
toeq b x y = b !! y !! x

--Find the points where the player will pass through.
toPoint :: (Int, Int) -> [(Int, Int)] -> Bool
toPoint (x, y) b = t
 where
  lb = filter (\(m, n) -> (m == x && n == y - 1) || (m == x + 2 && n == y)) b
  rb = filter (\(m, n) -> (m == x - 2 && n == y) || (m == x && n == y - 1)) b
  tl = filter (\(m, n) -> (m == x + 2 && n == y) || (m == x && n == y + 1)) b
  tr = filter (\(m, n) -> (m == x - 2 && n == y) || (m == x && n == y + 1)) b
  t =
    (length lb == 2) || (length rb == 2) || (length tl == 2) || (length tr == 2)

--Clear unnecessary properties
noDup :: (Int, Int) -> [(Int, Int)] -> Bool
noDup (x, y) b = t
 where
  l =
    filter (\(m, n) -> (m >= x - 2 && m <= x + 2) && (n >= y - 1 && n <= y)) b
  t = (length l < 5)

--Create map coordinate points.
toGraph :: [String] -> Char -> [(Int, Int)]
toGraph b c = ll
 where
  w  = (length (b !! 0)) - 1
  h  = (length b) - 1
  g  = [ (x, y) | x <- [0 .. w], y <- [0 .. h] ]
  l  = filter (\(n, m) -> toeq b n m == c) g
  lt = filter (\n -> noDup n l) l
  lg = filter (\n -> toPoint n lt) lt
  ll = sort $ nub lg

--check player can go from one point to another or not in vertical direction.
doNeig :: String -> Int -> Int -> Bool
doNeig s x1 x2 | x1 == x2  = False
               | otherwise = t
 where
  n = take (x2 - x1) $ drop x1 s
  m = filter (== '1') n
  t = length (m) == x2 - x1

--check player can go from one point to another or not in horizontal direction.
doNeigx :: String -> Int -> Int -> Bool
doNeigx s x1 x2 | x1 == x2  = False
                | otherwise = t
 where
  n = take (x2 - x1 - 2) $ drop (x1 + 2) s
  m = filter (== '1') n
  t = length (m) == (x2 - x1) `div` 2 - 1

--check 2 points are connected by path or not.
doConn :: [String] -> ((Int, Int), (Int, Int)) -> Bool
doConn b ((m1, n1), (m2, n2)) | m1 == m2  = sx
                              | n1 == n2  = sy
                              | otherwise = False
 where
  [(x1, y1), (x2, y2)] = sort [(m1, n1), (m2, n2)]
  sx = if (x1 == x2) then doNeig (map (!! x1) b) y1 y2 else False
  sy = if (y1 == y2) then doNeigx (b !! y1) x1 x2 else False

--Sub-function to solve a map.
toMap :: [String] -> [((Int, Int), (Int, Int))]
toMap b = m4
 where
  m1 = toGraph b '1'
  m2 = ([startPos b] ++ m1 ++ [endPos b])
  m3 = [ (x, y) | x <- m2, y <- m2 ]
  m4 = filter (\x -> doConn b x) m3

--Sub-function to solve a map.
toSearch
  :: [((Int, Int), (Int, Int))]
  -> (Int, Int)
  -> (Int, Int)
  -> [((Int, Int), Bool)]
toSearch m (sx, sy) (ex, ey)
  | ex == fex && ey == fey = [((ex, ey), True)]
  | otherwise = ((fex, fey), False) : toSearch m (fex, fey) (ex, ey)
 where
  np1 = filter (\((bx, by), (ex, ey)) -> sx == bx && sy == by) m
  np2 = head $ sort np1
  ((fbx, fby), (fex, fey)) = np2

--Computing an (ideally optimal) solution.
genPath :: [String] -> [((Int, Int), Bool)] -> String
genPath b tl
  | up && not cond = "Up -> " ++ genPath b (drop 1 tl)
  | down && not cond = "Down -> " ++ genPath b (drop 1 tl)
  | right && not cond = "Right -> " ++ genPath b (drop 1 tl)
  | left && not cond = "Left -> " ++ genPath b (drop 1 tl)
  | up && cond = "Cond{" ++ [colo] ++ "}{Up} -> " ++ genPath b (drop 1 tl)
  | down && cond = "Cond{" ++ [colo] ++ "}{Down} -> " ++ genPath b (drop 1 tl)
  | right && cond = "Cond{" ++ [colo] ++ "}{Right} -> " ++ genPath b (drop 1 tl)
  | left && cond = "Cond{" ++ [colo] ++ "}{Left -> " ++ genPath b (drop 1 tl)
  | otherwise = " End "
 where
  t              = length tl >= 2
  ((x1, y1), t1) = if t then tl !! 0 else ((0, 0), False)
  ((x2, y2), t2) = if t then tl !! 1 else ((0, 0), False)
  up             = t && (x1 == x2) && (y1 > y2)
  down           = t && (x1 == x2) && (y1 < y2)
  right          = t && (x1 < x2) && (y1 == y2)
  left           = t && (x1 > x2) && (y1 == y2)
  colo           = (b !! y1) !! x1
  cond           = elem colo "oyp"


--Accepting player's command to play game.
playUntil :: ([String], Mapattr) -> String -> IO ()
playUntil board str = do
  let x = words str
  if ((x !! 0) == "load") && (length x) == 2
    then do
      newb <- readcontent (x !! 1)
      putStrLn ("Read map succeessfully!")
      putStrLn ("Initial:")
      mapM_ putStrLn $ newb
      putStr ("> ")
      input <- getLine
      playUntil (newb, (snd board)) input
    else if check str "play" == 0
      then do
        let newTuple = doMove board str
        let newb     = fst $ fst newTuple
        let attr     = snd $ fst newTuple
        let oldattr  = snd $ board
        let oldbo    = bonus oldattr
        let newbo    = bonus attr
        outInfo (snd newTuple) str
        mapM_ putStrLn newb
        bonusShow oldbo newbo
        winExit (win attr)
        putStr ("> ")
        input <- getLine
        playUntil (newb, attr) input
      else if check str "quit" == 0
        then do
          putStrLn ("Exit")
          return ()
        else if check str "check" == 0
          then do
            putStrLn ("Checking...")
            let chkb = doFind board
            --mapM_ putStrLn $ fst chkb 
            showCheck $ win $ snd chkb
            putStr (">")
            input <- getLine
            playUntil board input
             --let (x1,y)=startPos $ fst board
             --let chkb=doGo' board [((x1,y),0)]
             --print chkb 
             --return()
          else if check str "solve" == 0
            then do
              putStrLn ("Solving...")
              let chkb = doFind board
              --mapM_ putStrLn $ fst chkb 
              showCheck $ win $ snd chkb
              unsolvExit $ win $ snd chkb
              let nnn = toMap (fst chkb) --toGraph (fst chkb) '1'
              --print nnn
              let ep  = endPos $ fst chkb
              let
                nny = ((ep), False)
                  : toSearch nnn (endPos $ fst chkb) (startPos $ fst chkb)
              --print nny
              let p = genPath (fst board) (reverse nny)
              putStrLn (p)
              putStr (">")
              input <- getLine
              playUntil board input
               --putStrLn("Exit")
               --return()
            else if check str "function" == 0
              then do
                putStrLn ("Function defined")
                let attr = snd board
                let x    = words str
                let b    = bonus $ attr
                let c    = color $ attr
                let d    = dir $ attr
                let w    = win $ attr
                let f    = drop 1 x
                putStr ("> ")
                input <- getLine
                playUntil
                  ( fst board
                  , Mapattr { bonus = (b)
                            , color = c
                            , win   = w
                            , dir   = d
                            , fun   = f
                            }
                  )
                  input
              else if ((x !! 0) == "generate")
                then doDis
                else if check str "hint" == 0
                  then do
                    let chkb = doFind board
                    --mapM_ putStrLn $ fst chkb
                    showCheck $ win $ snd chkb
                    unsolvExit $ win $ snd chkb
                    let nnn = toMap (fst chkb) --toGraph (fst chkb) '1'
                    --print nnn
                    let ep = endPos $ fst chkb
                    let
                      nny = ((ep), False) : toSearch nnn
                                                     (endPos $ fst chkb)
                                                     (startPos $ fst chkb)
                    --print nny
                    let p = genPath (fst board) (reverse nny)
                    putStr ("hint: ")
                    putStrLn ((words p) !! 0)
                    putStr (">")
                    input <- getLine
                    playUntil board input
                  else do
                    let [cmd] = take 1 $ words str
                    let w     = check str cmd
                    let err = if w == 0 then "command" else "word " ++ show w
                    putStrLn ("Wrong input at " ++ err)
                    putStr ("> ")
                    input <- getLine
                    playUntil board input

--Draw an integer randomly range from x to y.
drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x, y))

--Draw zig-zag path.
roadline :: Int -> Int -> [(Int, Int)]
roadline w h = [(0, y1), (x1, y1), (x1, y2), (w - 1, y2)]
 where
  x1 = unsafePerformIO (drawInt 5 (w - 5))
  y1 = unsafePerformIO (drawInt 2 (h - 2))
  y2 = unsafePerformIO (drawInt 2 (h - 2))

--Draw a box on map.
roadbox :: Int -> Int -> [(Int, Int)]
roadbox w h = [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)]
 where
  x1 = unsafePerformIO (drawInt 1 (w `div` 2))
  y1 = unsafePerformIO (drawInt 2 ((h `div` 3) + 2))
  x2 = unsafePerformIO (drawInt (w `div` 2) (w - 2))
  y2 = unsafePerformIO (drawInt ((h `div` 2) + 3) (h - 2))

--Generate a map completely covered with grass.
allGrassMap :: Int -> Int -> [String]
allGrassMap w h = replicate h (replicate w '*')

--get the coordinate of path.
toCoor :: [String] -> Char -> [(Int, Int)]
toCoor b c = l
 where
  w = (length (b !! 0)) - 1
  h = (length b) - 1
  g = [ (x, y) | x <- [0 .. w], y <- [0 .. h] ]
  l = filter (\(n, m) -> (b !! m) !! n == c) g

--Find the points where road branches.
doCr :: [String] -> Int -> Int -> Char -> Bool
doCr b x y c = tag
 where
  w   = (length (b !! 0)) - 1
  h   = (length b) - 1
  u   = if y >= 1 && (b !! (y - 1) !! x) == c then 1 else 0
  d   = if y < h && ((b !! (y + 1)) !! x) == c then 1 else 0
  r   = if x < w && ((b !! y) !! (x + 1)) == c then 1 else 0
  l   = if x >= 1 && ((b !! y) !! (x - 1)) == c then 1 else 0
  tag = (u + d + r + l) > 3


--Find the branch coordinate.
toCross :: [String] -> Char -> [(Int, Int)]
toCross b c = l
 where
  w = (length (b !! 0)) - 1
  h = (length b) - 1
  g = [ (x, y) | x <- [0 .. w], y <- [0 .. h] ]
  l = filter (\(n, m) -> doCr b n m c) g

--Add colour block to points where road branches.
addColor :: [String] -> [(Int, Int)] -> [String]
addColor b cl | length cl == 0 = b
              | otherwise      = n2
 where
  [(x, y)] = take 1 cl
  w        = (x + y) `mod` 3
  c        = "oyP" !! w
  n1       = put2d x y c b
  n2       = addColor n1 (drop 1 cl)

--Generate a map.
genMap :: Int -> Int -> [String]
genMap w h = gb6
 where
  l   = roadline w h
  xf  = (fst (l !! (length l - 1)))
  yf  = (snd (l !! (length l - 1)))
  r   = allGrassMap w h
  gl  = drawMap r l
  b   = roadbox w h
  gb1 = drawMap gl b
  gb2 = put2d (fst (l !! 0)) (snd (l !! 0)) '@' gb1
  gb3 = put2d xf yf 't' gb2
  m   = toCoor gb3 '-'
  bp1 = (length m) `div` 2 -- drawInt' 1 ((length m)-1)
  bp2 = (length m) `div` 3 -- drawInt' 1 ((length m)-1)
  bw1 = m !! bp1
  bw2 = m !! bp2
  gb4 = put2d (fst bw1) (snd bw1) 'b' gb3
  gb5 = put2d (fst bw2) (snd bw2) 'b' gb4
  cl  = toCross gb3 '-'
  gb6 = addColor gb5 cl


--Sub-function of genMap.
drawMap :: [String] -> [(Int, Int)] -> [String]
drawMap m pts | hzn && co = mh
              | vtl && co = mv
              | otherwise = m
 where
  co                   = length pts >= 2
  [(x1, y1), (x2, y2)] = if co then take 2 pts else [(1, 2), (3, 4)]
  hzn                  = if y1 == y2 then True else False
  vtl                  = if x1 == x2 then True else False
  mh                   = drawhzn m pts
  mv                   = drawvtl m pts

--Draw horizontal road on map.
drawhzn :: [String] -> [(Int, Int)] -> [String]
drawhzn map pts | tag       = map
                | otherwise = lasMap
 where
  tag                  = ((length pts) < 2) || (y1 /= y2)
  [(x1, y1), (x2, y2)] = sort $ take 2 pts
  tmpl =
    (take x1 (map !! y2))
      ++ (replicate (x2 - x1 + 1) '-')
      ++ (drop (x2 + 1) (map !! y2))
  tMap   = (take y1 map) ++ [tmpl] ++ (drop (y1 + 1) map)
  lasMap = drawMap tMap (drop 1 pts)

--Draw vertical road on map.
drawvtl :: [String] -> [(Int, Int)] -> [String]
drawvtl map pts | tag       = map
                | otherwise = lasMap
 where
  nm                   = doSwap map
  tag                  = (length pts < 2) || (y1 /= y2)
  [(y1, x1), (y2, x2)] = take 2 pts
  [(x3, y3), (x4, y4)] = sort [(x1, y1), (x2, y2)]
  tmpl =
    (take x3 (nm !! y4))
      ++ (replicate (x4 - x3 + 1) '-')
      ++ (drop (x4 + 1) (nm !! y4))
  tMap   = (take y4 nm) ++ [tmpl] ++ (drop (y4 + 1) nm)
  tmMap  = doSwap tMap
  lasMap = drawMap tmMap (drop 1 pts)

swapXY [(x1, y1), (x2, y2)] = [(y1, x1), (y2, x2)]

--Display generated map.
doDis = mapM_ putStrLn $ map (intersperse ' ') $ genMap 30 16


--start of prgram.
main = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  line <- getLine
  let iniAttr =
        Mapattr { bonus = 0, color = ' ', win = False, dir = [], fun = [] }
  playUntil ([], iniAttr) line
