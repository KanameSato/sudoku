module Data.Sudoku where

import Data.List as DL
import Util.Func as UF

data Cell = Fixed Int | Random

type Sudoku = [[Cell]]


isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _ = False

isRandom :: Cell -> Bool
isRandom = not . isFixed

fixedValue :: Cell -> Int
fixedValue (Fixed v)  = v
fixedValue (Random l) = error $ "nothing value. Cell condition is " ++ show l


-- 一意性に矛盾があればTrueを返す
-- 全てのセルがFixedで、縦横とブロックごとの一意性を検証する
isFailure :: Sudoku -> Bool
isFailure sudoku = linesIsUnique && blockIsUnique where
  valueIsUnique :: [Cell] -> Bool
  valueIsUnique = isUnique . map fixedValue . filter isFixed

  -- 縦横の一意チェック
  linesIsUnique = all valueIsUnique sudoku && all valueIsUnique (DL.transpose sudoku)

  -- ブロックの一意チェック
  blockIsUnique = all valueIsUnique (splitBlock 3 sudoku)


-- 数独が完成しているかを判定する
-- 全てのセルがFixedで、一意性に矛盾がなければTrueを返す
isComplete :: Sudoku -> Bool
isComplete sudoku = allFixed && not (isFailure sudoku) where
  allFixed   = all isFixed $ concat sudoku


solve :: Sudoku -> Maybe Sudoku
solve sudoku
  | isComplete sudoku = Just sudoku
  | otherwise         = undefined
