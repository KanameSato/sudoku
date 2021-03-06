module Main where

import Control.Monad
import Data.Maybe
import Data.Sudoku
import Data.FList
import Control.Concurrent.Chan

main :: IO ()
main = do
  lst <- replicateM 9 getLine
  let sudokuList = map (mapMaybe charToCell) lst
  let sudoku = fromList . concat $ sudokuList
  putStrLn "solving..."
  forM_ (solveAll sudoku) (print . showSudoku)
