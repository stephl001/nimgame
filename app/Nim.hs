module Nim (newGame, resume, play, Game (..), Board, BoardRow, Player, Move) where

import Data.Function ((&))
import Data.List (intercalate, intersperse)

data Player = Player1 | Player2

type Winner = Player

type StarCount = Int

newtype BoardRow = BoardRow StarCount

newtype Board = Board [BoardRow]

type Row = Int

type Stars = Int

type Move = (Row, Stars)

data Game
  = Playing Board Player
  | Over Board Winner
  | InvalidRow Board Player Row
  | InvalidStarCount Board Player Row Stars

instance Show BoardRow where
  show (BoardRow starCount) = replicate starCount '*' & intersperse ' '

instance Show Player where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

instance Show Board where
  show (Board rows) =
    zipWith (\i r -> show i ++ ": " ++ show r) [1 ..] rows
      & intercalate "\n"

instance Show Game where
  show (Playing board player) = show board ++ "\n" ++ show player
  show (Over board winner) = show board ++ "\n" ++ show winner ++ " wins!"
  show (InvalidRow board player row) = "Invalid Row: " ++ show row
  show (InvalidStarCount board player row stars) = "Invalid star Count: " ++ show stars

initial :: Board
initial = map BoardRow [5, 4 .. 1] & Board

swapPlayer :: Player -> Player
swapPlayer Player1 = Player2
swapPlayer Player2 = Player1

newGame :: Game
newGame = Playing initial Player1

isValidRowMove :: Board -> Row -> Bool
isValidRowMove (Board rows) r = r >= 1 && r <= length rows

rowStarCount :: BoardRow -> Int
rowStarCount (BoardRow sc) = sc

isValidStarMove :: Board -> Row -> StarCount -> Bool
isValidStarMove (Board rows) r sc =
  isValidRowMove (Board rows) r && sc > 0 && rowStarCount (rows !! (r - 1)) >= sc

resume :: Game -> Game
resume (Over _ _) = error "Invalid game state"
resume g@(Playing _ _) = g
resume (InvalidRow b p _) = Playing b p
resume (InvalidStarCount b p _ _) = Playing b p

updateRow :: Move -> Int -> BoardRow -> BoardRow
updateRow (row, sc) rowIndex br@(BoardRow stars)
  | row == rowIndex = BoardRow (stars - sc)
  | otherwise = br

updateBoard :: Board -> Move -> Board
updateBoard (Board rows) move =
  zipWith (updateRow move) [1 ..] rows & Board

isRowEmpty :: BoardRow -> Bool
isRowEmpty (BoardRow starCount) = starCount == 0

isBoardEmpty :: Board -> Bool
isBoardEmpty (Board rows) = all isRowEmpty rows

play :: Game -> Move -> Game
play (Playing b p) m@(row, starCount)
  | not (isValidRowMove b row) = InvalidRow b p row
  | not (isValidStarMove b row starCount) = InvalidStarCount b p row starCount
  | otherwise =
      if isBoardEmpty updatedBoard then Over updatedBoard p else Playing updatedBoard (swapPlayer p)
  where
    updatedBoard = updateBoard b m
