module Data.FList where

import Safe.Func as S
import Data.Maybe


-- 特定の要素にフォーカスを合わせたリスト
data FList a = FList { reverseFront :: [a], currentMaybe :: Maybe a, back :: [a] }

instance (Eq a) => Eq (FList a) where
  (FList a1 b1 c1) == (FList a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2

-- 空リストでなければ先頭要素にフォーカスを合わせる
fromList :: [a] -> FList a
fromList [] = FList [] Nothing []
fromList (x:xs) = FList [] (Just x) xs

-- 要素がない場合はエラー
current :: FList a -> a
current (FList _ (Just x) _) = x
current _ = error "nothing element"

-- フォーカスされている要素に関数を適用する
-- どの要素もフォーカスされていなければ何もしない
update :: (a -> a) -> FList a -> FList a
update f (FList a b c) = FList a (fmap f b) c


next :: FList a -> FList a
next (FList a (Just b) (c:cs)) = FList (b:a) (Just c) cs
next (FList a Nothing  (c:cs)) = FList a     (Just c) cs
next (FList a (Just b) _)      = FList (b:a) Nothing []
next fl = fl

prev :: FList a -> FList a
prev (FList (a:as) (Just b) c) = FList as (Just a) (b:c)
prev (FList (a:as) Nothing  c) = FList as (Just a) c
prev (FList _      (Just b) c) = FList [] Nothing  (b:c)
prev fl = fl

nextN :: Int -> FList a -> FList a
nextN n (FList prev currentMaybe back) = FList (addprevs++maybeToList currentMaybe++prev) nextCurrent nextBack where
  (moveElems, nextBack) = splitAt n back
  reverseMoveElems = reverse moveElems
  addprevs = S.tail reverseMoveElems
  nextCurrent = S.headMaybe reverseMoveElems

prevN :: Int -> FList a -> FList a
prevN n (FList prev currentMaybe back) = FList nextPrev nextCurrent (addnexts++maybeToList currentMaybe++back) where
  (reverseMoveElems, nextPrev) = splitAt n prev
  moveElems = reverse reverseMoveElems
  addnexts = S.tail moveElems
  nextCurrent = S.headMaybe moveElems


nextN' :: Int -> FList a -> FList a
nextN' 0 fl = fl
nextN' n fl = nextN' (n-1) (next fl)

prevN' :: Int -> FList a -> FList a
prevN' 0 fl = fl
prevN' n fl = prevN' (n-1) (prev fl)