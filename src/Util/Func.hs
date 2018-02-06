module Util.Func where

import System.Random
import GHC.Exts
import Data.List as DL

-- リスト内に重複要素がなければTrueを返す
isUnique :: (Eq a) => [a] -> Bool
isUnique lst = isUnique' lst [] where
  isUnique' :: (Eq a) => [a] -> [a] -> Bool
  isUnique' []     _     = True
  isUnique' (x:xs) elems
    | x `elem` elems = False
    | otherwise      = isUnique' xs (x:elems)


-- リストをnの倍数の要素ごとに分割する
splitMulti :: Int -> [a] -> [[a]]
splitMulti _ [] = []
splitMulti n lst = current:splitMulti n next where
  (current, next) = splitAt n lst


-- 二重リストをn * nのブロックに分割する
-- n = 0の場合はエラー
--
-- splitBlock 2 [
--                [a, b, c, d],
--                [e, f, g, h],
--                [i, j, k, l, m],
--                [n, o, p]
--              ]
-- は
--              [
--                [a, b, e, f],
--                [c, d, g, h],
--                [i, j, n, o],
--                [k, l, p],
--                [m]
--              ]
-- を返す
splitBlock :: Int -> [[a]] -> [[a]]
splitBlock 0 _  = error "Can't split block size '0'"
splitBlock _ [] = []
splitBlock n lst = (map concat . DL.transpose) splittedTarget ++ splitBlock n later where
  (target, later) = splitAt n lst
  splittedTarget = map (splitMulti n) target


rotateL :: [a] -> [a]
rotateL [] = []
rotateL (x:xs) = xs++[x]

shuffle :: StdGen -> [a] -> [a]
shuffle gen lst = map fst . sortWith snd . zip lst $ (chain random gen :: [Int])

chain :: (a -> (b, a)) -> a -> [b]
chain f g = value:chain f nextGen where
  (value, nextGen) = f g
