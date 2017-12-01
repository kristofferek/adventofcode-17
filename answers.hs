-- 1A
sumOfAdjecents :: [Integer] -> Integer
sumOfAdjecents l = sumOfAdjecents' (head l) l
  where
    sumOfAdjecents' f [x] = if x == f then x else 0
    sumOfAdjecents' f (x:xs) | x == head xs = x + sumOfAdjecents' f xs
                             | otherwise    = sumOfAdjecents' f xs

-- 1B
sumOfOpposite :: [Integer] -> Integer
sumOfOpposite c = (*2) $ sumOfOpposite' cStart cEnd
  where
    (cStart, cEnd) = splitAt (length c `div` 2) c
    sumOfOpposite' [] [] = 0
    sumOfOpposite' (s:ss) (e:es) | s == e    = s + sumOfOpposite' ss es
                                 | otherwise = sumOfOpposite' ss es
