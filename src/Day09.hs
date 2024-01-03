module Day09 (solution) where

import Lib (InputFileContent, Solution)

solution :: Solution
solution = (part1, part2)


---------- PART ONE ----------

extrapolate :: [Int] -> Int
extrapolate [x] = x
extrapolate xxs@(x : xs)
  | all (== 0) ds = x
  | otherwise     = last xxs + extrapolate ds
  where ds = zipWith (-) xs xxs

part1 :: InputFileContent -> Int
part1 s = sum $ map extrapolate xss
  where ls  = lines s
        wss = map words ls
        xss = map (map read) wss


---------- PART TWO ----------

extrapolate' :: [Int] -> Int
extrapolate' [x] = x
extrapolate' xxs@(x : xs)
  | all (== 0) ds = x
  | otherwise     = x - extrapolate' ds
  where ds = zipWith (-) xs xxs

part2 :: InputFileContent -> Int
part2 s = sum $ map extrapolate' xss
  where ls  = lines s
        wss = map words ls
        xss = map (map read) wss
