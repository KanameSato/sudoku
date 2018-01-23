module Safe.Func where

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x

tailMaybe :: [a] -> Maybe [a]
tailMaybe []     = Nothing
tailMaybe (_:xs) = Just xs

tail :: [a] -> [a]
tail [] = []
tail (_:xs) = xs



