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

-- 5B
-- Done in java (See fiveB.java)

input = [2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14] :: [Int]
-- 6A
memReal :: [Int] -> Int
memReal a = memReal' [] a 0
  where
    memReal' old l t | any (isPrefixOf l) old = t
                     | otherwise = memReal' (l:old) (zipWith (+) l ones) (t+1)
      where
        ones = replicate (maxVal-(4-maxIndex)) 1
                ++ replicate (4-maxVal) 0
                ++ replicate (min (4-maxIndex) maxVal) 1
        (maxVal, maxIndex) = maximum $ zip l [0..]
