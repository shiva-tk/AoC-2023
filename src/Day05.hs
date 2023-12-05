module Day05 (solution) where

import Data.Function (on)
import Data.List (sort, sortBy)
import Lib (InputFileContent, Solution, dropPrefixMaybe, splitOn)

import Debug.Trace

solution :: Solution
solution = (part1, part2)


---------- PART ONE ----------

mapping :: [String] -> [(Int, Int, Int)]
mapping ss = ms'
  where
    wss  = map words ss
    wss' = filter ((3 <=) . length) wss
    ms   = map (\(dst : src : n : _) -> (read dst, read src, read n)) wss'
    ms'  = sort ms

mappings :: [String] -> [[(Int, Int, Int)]]
mappings = map mapping . splitOn ""

seeds :: String -> [Int]
seeds = map read . tail . words

parseInput :: InputFileContent -> ([Int], [[(Int, Int, Int)]])
parseInput s = (seeds ss, mappings ms)
  where (ss: ms) = lines s

destination :: Int -> [(Int, Int, Int)] -> Int
destination src [] = src
destination src ((dst, src', n): xs)
  | src' <= src && src < src' + n = dst + (src - src')
  | otherwise                     = destination src xs

traverseMappings :: Int -> [[(Int, Int, Int)]] -> Int
traverseMappings = foldl destination 

part1 :: InputFileContent -> Int
part1 s = minimum [traverseMappings s ms | s <- ss]
  where (ss, ms) = parseInput s


---------- PART ONE ----------

seeds' :: String -> [(Int, Int)]
seeds'  s = pairRanges ns
  where (_ : s') = words s
        ns = map read s'
        pairRanges :: [Int] -> [(Int, Int)]
        pairRanges [] = []
        pairRanges (s : n : ss) = (s, s + n - 1) : pairRanges ss

mapping' :: [String] -> [(Int, Int, Int, Int)]
mapping' ss = ms'
  where
    wss  = map words ss
    wss' = filter ((3 <=) . length) wss
    ms   = map (\(dst : src : n : _) -> (read dst, read src, read n)) wss'
    ms'  = map (\(dst, src, n) -> (src, src + n - 1, dst, dst + n - 1)) ms

mappings' :: [String] -> [[(Int, Int, Int, Int)]]
mappings' = map mapping' . splitOn ""

dsts :: (Int, Int, Int, Int) -> (Int, Int)
dsts (src, src', dst, dst') = (dst, dst')

srcs :: (Int, Int, Int, Int) -> (Int, Int)
srcs (src, src', dst, dst') = (src, src')

parseInput' :: InputFileContent -> ([(Int, Int)], [[(Int, Int, Int, Int)]])
parseInput' s = (seeds' ss, mappings' ms)
  where (ss: ms) = lines s

destination' :: (Int, Int) -> [(Int, Int, Int, Int)] -> [(Int, Int)]
destination' se m = go se m'
  where 
    m' = sort m
    
    go :: (Int, Int) -> [(Int, Int, Int, Int)] -> [(Int, Int)]
    go (s, e) [] = [(s, e)]
    go (s, e) ((ss, se, ds, de) : m)
      | e < ss = [(s, e)]
      | se < s = go (s, e) m
      | s < ss = (s, ss - 1) :  go (ss, e) ((ss, se, ds, de) : m)
      | ss < s = go (s, e) ((s, se, ds + s - ss, de) : m)
      | ss == s = (ds, de') : remaining
      where
        e' = min e se
        de' = ds + e' - s
        remaining = if e' == e then [] else go (e' + 1, e) ((ss, se, ds, de) : m)

traverseMappings' :: [(Int, Int)] -> [[(Int, Int, Int, Int)]] -> [(Int, Int)]
traverseMappings' = foldl (\ss m -> concatMap (`destination'` m) ss)

part2 :: InputFileContent -> Int
part2 s = minimum $ map fst $ traverseMappings' ss ms
  where (ss, ms) = parseInput' s
