module Day3 (part1, part2) where

import Data.Array
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Lib (InputFileContent, makeArray)


---------- PART ONE  -----------

indexedNumbers :: String -> [((Int, Int), Int)]
indexedNumbers s = go 0 s
  where
    go :: Int -> String -> [((Int, Int), Int)]
    go _ "" = []
    go i s
      | null d     = go (j + 1) w
      | otherwise  = ((i', j), n) : go (j + 1) w 
      where (u, v) = break isDigit s
            (d, w) = span isDigit v
            i'     = i + length u
            j      = i' + length d - 1
            n      = read d

partNumbers :: String -> [Int]
partNumbers schematic = ns'
  where 
    acs = makeArray schematic
    (_, (m, n)) = bounds acs

    ls   = lines schematic
    jnss = map indexedNumbers ls
    ijns =  concat $ zipWith (\i jns -> map (\((j, j'), n) -> ((i, j, j'), n)) jns) [0..] jnss
    ns   = filter (\((i, j, j'), _) -> isPartNumber i j j') ijns
    ns'  = map (\((_, _, _), n) -> n) ns

    isPartNumber :: Int -> Int -> Int -> Bool
    isPartNumber i j j' = any (isSymbol . (acs !)) toSearch
      where 
        l = [(i, j - 1) | j > 0]
        r = [(i, j' + 1) | j' < n - 1]
        u = [(i - 1, j'') | i > 0, j'' <- [j .. j']]
        d = [(i + 1, j'') | i < m - 1 , j'' <- [j .. j']]
        ul = [(i - 1, j - 1) | i > 0, j > 0]
        ur = [(i - 1, j' + 1) | i > 0, j' < n - 1]
        dl = [(i + 1, j - 1) | i < m - 1, j > 0]
        dr = [(i + 1, j' + 1) | i < m - 1, j' < n - 1]
        toSearch = concat [l, r, u, d, ul, ur, dl, dr]
        isSymbol c = not (isDigit c) && (c /= '.')

part1 :: InputFileContent -> Int
part1 f = sum (partNumbers f)


---------- PART TWO ----------

indexedGears :: String -> [Int]
indexedGears s = go 0 s
  where
    go :: Int -> String -> [Int]
    go _ "" = []
    go i ('*' : s) = i : go (i + 1) s
    go i (_ : s) = go (i + 1) s

merge :: [(Int, Int)] -> [(Int, Int)]
merge []            = []
merge (ij : ijs)    = ij : merge (filter (not . nextTo ij) ijs)

nextTo :: (Int, Int) -> (Int, Int) -> Bool
nextTo (i, j) (i', j') = abs (j - j') == 1 && i == i'

gearRatios :: String -> [Int]
gearRatios schematics = mapMaybe (uncurry (gearRatio acs)) ijs
  where 
    acs = makeArray schematics
    (_, (m, n)) = bounds acs

    ls  = lines schematics
    jss = map indexedGears ls
    ijs = concat $ zipWith (\i js -> map (\j -> (i, j)) js) [0..] jss

gearRatio :: Array (Int, Int) Char -> Int -> Int -> Maybe Int
gearRatio acs i j
  | length adjacentParts == 2 = Just (product adjacentPartNumbers)
  | otherwise                 = Nothing
  where
    (_, (m, n))         = bounds acs
    u                   = [(i - 1, j) | i > 0]
    d                   = [(i + 1, j) | i < m - 1]
    l                   = [(i, j - 1) | j > 0]
    r                   = [(i, j + 1) | j < n - 1]
    ul                  = [(i - 1, j - 1) | i > 0, j > 0]
    ur                  = [(i - 1, j + 1) | i > 0, j < n - 1]
    dl                  = [(i + 1, j - 1) | i < m - 1, j > 0]
    dr                  = [(i + 1, j + 1) | i < m - 1, j < n - 1]
    adjacents           = concat [u, d, l, r, ul, ur, dl, dr]
    adjacentParts       = merge (filter (isDigit . (acs !)) adjacents)
    adjacentPartNumbers = map (uncurry (numberAt acs)) adjacentParts

numberAt :: Array (Int, Int) Char -> Int -> Int -> Int
numberAt acs i j = go j j
  where
    go :: Int -> Int -> Int
    go j j' 
      | j  - 1 >= 0 && isDigit (acs ! (i, j  - 1)) = go (j - 1) j'
      | j' + 1 <  m && isDigit (acs ! (i, j' + 1)) = go j (j' + 1)
      | otherwise = read s
      where
        s = [acs ! (i, j'') | j'' <- [j .. j']]
        (_, (m, _)) = bounds acs

part2 :: InputFileContent -> Int
part2 f = sum (gearRatios f)
