module Data.Sudoku where

import qualified Data.Maybe as DM
import qualified Data.List  as DL
import qualified Util.Func  as UF
import qualified Data.FList as DF
import qualified Data.Function as DFun
import qualified Data.Char  as DC
import Debug.Trace

data Cell = Fixed Int | Random deriving Eq

type Sudoku = DF.FList Cell

charToCell :: Char -> Maybe Cell
charToCell '?' = Just Random
charToCell c
  | '1' <= c && c <= '9' = Just . Fixed $ ((-) `DFun.on` DC.ord) c '0'
  | otherwise            = Nothing

instance Show Cell where
  show (Fixed n) = show n
  show Random    = "?"

showSudoku :: Sudoku -> String
showSudoku = show . toList


toList :: Sudoku -> [[Cell]]
toList = UF.splitMulti 9 . DF.toList

isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _ = False

isRandom :: Cell -> Bool
isRandom = not . isFixed

fixedValue :: Cell -> Int
fixedValue (Fixed v)  = v
fixedValue Random = error "nothing value"


-- 一意性に矛盾があればTrueを返す
-- 全てのセルがFixedで、縦横とブロックごとの一意性を検証する
isFailure :: Sudoku -> Bool
isFailure sudoku = not $ linesIsUnique && vlinesIsUnique && blockIsUnique where
  valueIsUnique :: [Cell] -> Bool
  valueIsUnique = UF.isUnique . map fixedValue . filter isFixed

  sudokuList = toList sudoku

  -- 縦横の一意チェック
  linesIsUnique = all valueIsUnique sudokuList
  vlinesIsUnique = all valueIsUnique (DL.transpose sudokuList)

  -- ブロックの一意チェック
  blockIsUnique = all valueIsUnique (UF.splitBlock 3 sudokuList)


-- 数独が完成しているかを判定する
-- 全てのセルがFixedで、一意性に矛盾がなければTrueを返す
isComplete :: Sudoku -> Bool
isComplete sudoku = allFixed && not (isFailure sudoku) where
  allFixed = all isFixed . concat . toList $ sudoku

solve :: Sudoku -> Maybe Sudoku
solve sudoku
  | isComplete sudoku = return sudoku
  | isFailure  sudoku = Nothing
  | DF.current sudoku == Random = DL.find isComplete . DM.mapMaybe (\x -> solve . DF.next . DF.update (const . Fixed $ x) $ sudoku) $ [1..9]
  | otherwise = solve . DF.next $ sudoku


solveAll :: Sudoku -> [Sudoku]
solveAll sudoku
  | isComplete sudoku = return sudoku
  | isFailure  sudoku = []
  | DF.current sudoku == Random = filter isComplete . DL.concatMap (\x -> solveAll . DF.next . DF.update (const . Fixed $ x) $ sudoku) $ [1..9]
  | otherwise = solveAll . DF.next $ sudoku

--  ?????????
--  ???????27
--  4??6?8???
--  ?71???3??
--  2385?6419
--  9641??75?
--  395?278??
--  182?6?974
--  ?468192?5

--  ???7?5???
--  ?5????6??
--  9??????4?
--  1????95??
--  ????23??8
--  ??????4??
--  59???6?3?
--  3??2?8???
--  ??2???76?

--  ?????????
--  ??36?????
--  ?7??9?2??
--  ?5???7???
--  ????457??
--  ???1???3?
--  ??1????68
--  ??85???1?
--  ?9????4??

--  ?????????
--  ?????????
--  ?????????
--  ?????????
--  ?????????
--  ?????????
--  ?????????
--  ?????????
--  ?????????




