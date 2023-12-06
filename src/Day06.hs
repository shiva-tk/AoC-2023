module Day06 where

import Lib (Solution, InputFileContent)

import Debug.Trace

solution :: Solution
solution = (part1, part2)

waysToWin :: Int -> Int -> Int
waysToWin tMax sMax = floor ub - ceiling lb + 1
  where
    k = sqrt $ fromIntegral (tMax ^ 2 - 4 * sMax)
    ub = (fromIntegral tMax + k) / 2
    lb = (fromIntegral tMax - k) / 2


---------- PART ONE ----------

parseTimes :: String -> [Int]
parseTimes = map read . tail .words

parseDistances :: String -> [Int]
parseDistances = map read . tail .words

part1 :: InputFileContent -> Int
part1 input = product (zipWith waysToWin ts' ss')
  where 
    (ts : ss : _) = lines input
    ts'           = parseTimes ts
    ss'           = parseDistances ss


---------- PART TWO ----------

parseTime :: String -> Int
parseTime = read . concat . tail .words

parseDistance :: String -> Int
parseDistance = read . concat . tail .words

part2 :: InputFileContent -> Int
part2 input = waysToWin t' s'
  where 
    (t : s : _) = lines input
    t' = parseTime t
    s' = parseDistance s
