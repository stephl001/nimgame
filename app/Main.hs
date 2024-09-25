module Main where

import Nim
import System.IO (hFlush, stdout)

main :: IO ()
main = playGame newGame

playGame :: Game -> IO ()
playGame g@(Playing b p) = do
  putStr (show g)
  newline 1
  move <- queryMove
  (playGame . play g) move
playGame g@(Over _ _) = do
  print g
playGame g = do
  print g
  (playGame . resume) g

queryMove :: IO Move
queryMove = do
  row <- inputNumber "Enter a row number: "
  stars <- inputNumber "Enter stars to remove: "
  return (row, stars)

newline :: Int -> IO ()
newline 0 = do return ()
newline n = do
  putChar '\n'
  newline (n - 1)

inputNumber :: String -> IO Int
inputNumber prompt = do
  putStr prompt
  hFlush stdout
  readLn :: IO Int
