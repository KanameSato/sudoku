module Data.FList where

import Safe.Func as S
import Data.Maybe


-- 特定の要素にフォーカスを合わせたリスト
data FList a = FList { reverseFront :: [a], currentMaybe :: Maybe a, back :: [a], idx :: Int }

-- フォーカス位置は比較しない
instance (Eq a) => Eq (FList a) where
  (FList a1 b1 c1 _) == (FList a2 b2 c2 _) = a1 == a2 && b1 == b2 && c1 == c2

instance Foldable FList where
  foldMap f = foldMap f . toList

-- 空リストでなければ先頭要素にフォーカスを合わせる
fromList :: [a] -> FList a
fromList [] = FList [] Nothing [] 0
fromList (x:xs) = FList [] (Just x) xs 0

toList :: FList a -> [a]
toList (FList rf cm bk _) = reverse rf++maybeToList cm++bk

-- 要素がない場合はエラー
current :: FList a -> a
current (FList _ (Just x) _ _) = x
current _ = error "nothing element"

hasCurrent :: FList a -> Bool
hasCurrent = isNothing . currentMaybe

-- フォーカスされている要素に関数を適用する
-- どの要素もフォーカスされていなければ何もしない
update :: (a -> a) -> FList a -> FList a
update f (FList a b c i) = FList a (fmap f b) c i

-- フォーカスされている要素を上書きする
-- どの要素もフォーカスされていなければ何もしない
overwrite :: a -> FList a -> FList a
overwrite v (FList a b c i) = FList a (fmap (const v) b) c i

-- idx位置にある要素に関数を適用する
-- idx位置に要素がなければ何もしない
updateByIdx :: (a -> a) -> Int -> FList a -> FList a
updateByIdx f idx fl@(FList a b c i)
  | idx == i = update f fl
  | idx <  i = updateByIdx f idx $ nextN (i-idx) fl
  | idx >  i = updateByIdx f idx $ prevN (idx-i) fl


next :: FList a -> FList a
next (FList a (Just b) (c:cs) i) = FList (b:a) (Just c) cs (i+1)
next (FList a Nothing  (c:cs) i) = FList a     (Just c) cs (i+1)
next (FList a (Just b) _ i)      = FList (b:a) Nothing []  (i+1)
next fl = fl

prev :: FList a -> FList a
prev (FList (a:as) (Just b) c i) = FList as (Just a) (b:c) (i-1)
prev (FList (a:as) Nothing  c i) = FList as (Just a) c     (i-1)
prev (FList _      (Just b) c i) = FList [] Nothing  (b:c) (i-1)
prev fl = fl

nextN :: Int -> FList a -> FList a
nextN 0 fl = fl
nextN n fl = nextN (n-1) (next fl)

prevN :: Int -> FList a -> FList a
prevN 0 fl = fl
prevN n fl = prevN (n-1) (prev fl)