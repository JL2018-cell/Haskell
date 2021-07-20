module TicTacToe (tictactoe) where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.List

import Text.Printf

data Player = PO | PX
  deriving (Show, Eq)
switchP :: Player -> Player
switchP PO = PX
switchP PX = PO
--Show invalid input (Done)
--Show draw (Done)
--Cannot repeat same position. (Done)
--at least 1 space between input

tictactoe :: IO ()
tictactoe = game temp

game :: [String]->IO ()
game board = do
  let newBoard = ["N","N","N","N","N","N","N","N","N","C"]
  displayBox newBoard
  playGame newBoard PO --play O starts first.

displayBox :: [String]->IO ()
displayBox board = do {seqn2 board [printf "%s" str | str<-box2 ]; putStr " |\n.---.---.---."}

seqn2 :: [String] -> [IO a] -> IO ()
seqn2 _ [] =  return ()
seqn2 [] _ =  return ()
seqn2 (y:ys) (x:xs) =  do {x;if y=="N" then (printf "%s" " ") else if y=="O" then (printf "%s" "O") else (printf "%s" "X"); (seqn2 ys xs)}

turnO :: String
turnO = "O MOVE"

turnX :: String
turnX = "X MOVE"

playerX :: String
playerX = "X"

playerO :: String
playerO = "O"

playGame :: [String]->Player->IO ()
playGame board player = if (winOrNot board) --Whether a player wins?
  then (if player==PO then printf "\n%s WINS\n" playerX else printf "\n%s WINS\n" playerO)
  else if (fullOrNot board) then --Whether the board is full of chess?
    putStrLn "\nDRAW"
  else do
    putStrLn "" >> if (player==PO) then (printf "%s\n" turnO) else (printf "%s\n" turnX)
    board' <- boardAction board player --Player puts chess on game board.
    displayBox board' --Show to game board and chess on screen.
    playGame board' (switchP player)


boardAction :: [String] -> Player -> IO [String]
boardAction board player = do
  position <- getLine --Read player's input.
  if ((enoughInput position 0 0) && (digitOnly position)) then --Judge whether input is valid.
    if (noSuperposition board position) then do --Judge whether a chess has already been target position of player.
      putStrLn "INVALID POSITION"
      boardAction board player
    else
      return (interpret2 board position player)
  else do 
    putStrLn "INVALID POSITION"
    boardAction board player

noSuperposition :: [String]->String->Bool
noSuperposition board position = ((board!!n) == "O") || ((board!!n) == "X")
  where xs = extract position
        n = ((xs!!0)-1)*3 + (xs!!1) - 1

--((((extract "1 2")!!0)-1)*3 + ((extract "1 2")!!1) - 1)

fullOrNot :: [String]->Bool
fullOrNot [x] = True
fullOrNot (x:xs) = (x=="O" || x=="X") && (fullOrNot xs)

interpret2 :: [String]->String->Player->[String]
interpret2 board (x:xs) player = placeChess (extract (x:xs)) board player

valid :: String->[Int]
valid xs = 
  if ((enoughInput xs 0 0) && (digitOnly xs)) then 
    (extract xs) 
  else [1,1]

showInvalid :: IO ()
showInvalid = putStrLn "INVALID INPUT"
{-
enoughInput :: String->Int->Bool
enoughInput [] count = (count==2)
enoughInput (x:xs) count = if (isDigit x) then 
                             (enoughInput xs (count+1))
                           else
                             (enoughInput xs count)
-}

enoughInput :: String->Int->Int->Bool
enoughInput [] count safebit = (count==2)
enoughInput (x:xs) count safebit = if (count==1 && x==' ' && safebit==0) then 
                                     (enoughInput xs count (safebit+1))
                                   else if (count==1 && x/=' ' && safebit==0) then
                                     False
                                   else if (x==' ') then
                                     (enoughInput xs count safebit)
                                   else if (isDigit x) then
                                     if (((ord x - ord '0') < 4) && ((ord x - ord '0') > 0)) then
                                       (enoughInput xs (count+1) safebit)
                                     else
                                       False
                                   else
                                     False


digitOnly :: String->Bool
digitOnly [] = True
digitOnly (x:xs) = ((x==' ') || (isDigit x)) && (digitOnly xs)

extract :: String->[Int]
extract [] = []
extract (x:xs) = if (isDigit x) then 
                   [(ord x) - (ord '0')] ++ (extract xs)
                 else
                   (extract xs)

placeChess :: [Int] -> [String] -> Player -> [String]
placeChess xs ys PO = (take n ys) ++ ["O"] ++ (drop (n+1) ys)
  where n = ((xs!!0)-1)*3 + (xs!!1) - 1
placeChess xs ys PX = (take n ys) ++ ["X"] ++ (drop (n+1) ys)
  where n = ((xs!!0)-1)*3 + (xs!!1) - 1


winOrNot :: [String]->Bool
winOrNot board = cond1 board || cond2 board || cond3 board || cond4 board || cond5 board || cond6 board || cond7 board || cond8 board

cond1 :: [String]->Bool
cond1 b = (((b!!0)=="X") || ((b!!0)=="O")) && ((b!!0)==(b!!1)) && ((b!!1)==(b!!2))

cond2 :: [String]->Bool
cond2 b = (((b!!3)=="X") || ((b!!3)=="O")) && ((b!!3)==(b!!4)) && ((b!!4)==(b!!5))

cond3 :: [String]->Bool
cond3 b = (((b!!6)=="X") || ((b!!6)=="O")) && ((b!!6)==(b!!7)) && ((b!!7)==(b!!8))

cond4 :: [String]->Bool
cond4 b = (((b!!0)=="X") || ((b!!0)=="O")) && ((b!!0)==(b!!3)) && ((b!!3)==(b!!6))

cond5 :: [String]->Bool
cond5 b = (((b!!1)=="X") || ((b!!1)=="O")) && ((b!!1)==(b!!4)) && ((b!!4)==(b!!7))

cond6 :: [String]->Bool
cond6 b = (((b!!2)=="X") || ((b!!2)=="O")) && ((b!!2)==(b!!5)) && ((b!!5)==(b!!8))

cond7 :: [String]->Bool
cond7 b = (((b!!0)=="X") || ((b!!0)=="O")) && ((b!!0)==(b!!4)) && ((b!!4)==(b!!8))

cond8 :: [String]->Bool
cond8 b = (((b!!2)=="X") || ((b!!2)=="O")) && ((b!!2)==(b!!4)) && ((b!!4)==(b!!6))

box2 :: [String]
box2 = [".---.---.---.\n| "," | "," | ", " |\n.---.---.---.\n| "," | "," | ", " |\n.---.---.---.\n| "," | "," | "]

temp :: [String]
temp = ["N","N","N","N","N","N","N","N","N","C"]

--Delete after debug
temp2 = ["O","O","X","N","N","N","N","X","O","C"]
