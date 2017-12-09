import Data.List.Split
import Data.List

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

-- 2A
checkSum :: [Integer] -> Integer
checkSum s = sum $ map rowDiff rows
  where
    rows = chunksOf 16 s
    rowDiff row = maximum row - minimum row

-- 2B
evenlyDivSum :: [Integer] -> Integer
evenlyDivSum s = sum $ map rowDiff rows ++ map revRowDiff rows
  where
    rows = chunksOf 16 s
    revRowDiff r = rowDiff (reverse r)
    rowDiff [] = 0
    rowDiff (x:xs) = rowDiff xs + sum [ x `div` y | y <- xs, x `mod` y == 0]

-- 3A
spiralDist :: Int -> Int
spiralDist i = spiralDist' i 3
  where
    spiralDist' i m | m*m > i   = circle + min innerDist ((m-1)-innerDist)
                    | otherwise = spiralDist' i (m+2)
      where
        circle = (m-1) `div` 2
        innerDist = (((m*m)-circle)-i) `mod` (m-1)

-- 4A
noDuplicates :: [[String]] -> Int
noDuplicates [] = 0
noDuplicates (row:xs) | length row == length (nub row) = 1 + noDuplicates xs
                      | otherwise = noDuplicates xs

-- 4B
noMutations :: [[String]] -> Int
noMutations [] = 0
noMutations (row:xs) | length row == length (nub row) && rowPermutations row == 0 = 1 + noMutations xs
                     | otherwise = noMutations xs

rowPermutations :: [String] -> Int
rowPermutations [] = 0
rowPermutations (w:ws) = rowPermutations ws +
                              length (permutations w `intersect` ws)

-- 5A
listJumps :: [Int] -> Int
listJumps l = listJumps' l 0 0
  where
    len = length l
    listJumps' l pos steps | pos < 0 || pos >= len = steps
                           | otherwise = listJumps' (incNth pos l) newPos (steps+1)
      where newPos = pos + (l !! pos)

incNth :: Int -> [Int] -> [Int]
incNth n (x:xs) | n == 0 = (x+1):xs
                | otherwise = x:incNth (n-1) xs

-- 9A
-- Allready removed all regEx(!.) from inputs/9.txt
streamProcess :: String -> Int
streamProcess s = countGroups 0 0 $ drop 1 $ remGarbage False s
  where
    remGarbage _ [] = []
    remGarbage True (x:xs) = if x=='>' then remGarbage False xs
                                       else remGarbage True xs
    remGarbage False (x:xs)= if x=='<' then remGarbage True xs
                                       else x:remGarbage False xs
countGroups :: Int -> Int -> String -> Int
countGroups _ val [] = val
countGroups kids val ('}':xs) = countGroups (kids-1) (val+kids+1) xs
countGroups kids val ('{':xs) = countGroups (kids+1) val xs
countGroups kids val (x:xs) = countGroups kids val xs

countGarbage :: String -> Int
countGarbage s = countGarbage' False s 0
  where
    countGarbage' _ [] c = c
    countGarbage' True (x:xs) c = if x=='>' then countGarbage' False xs c
                                            else countGarbage' True xs (c+1)
    countGarbage' False (x:xs) c = if x=='<' then countGarbage' True xs c
                                             else countGarbage' False xs c
