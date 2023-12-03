module Day01 (solution) where

import GHC.Base  -- <|> (a.k.a mplus)
import Data.Char (isDigit, digitToInt)
import Data.List (elemIndex, findIndex, isPrefixOf)
import Lib (Solution, InputFileContent, digitToIntMaybe)

solution :: Solution
solution = (part1, part2)

---------- PART ONE ----------

firstLast :: String -> (Maybe Int, Maybe Int)
firstLast ""       = (Nothing, Nothing)
firstLast (c : cs) = (n <|> f, l <|> n)
  where
    (f, l) = firstLast cs
    n      = digitToIntMaybe c

calibrationValue :: String -> Int
-- Pre: String contains at least two digits
calibrationValue s = 10 * f + l
  where (Just f, Just l) = firstLast s

calibrationValues :: String -> [Int]
calibrationValues = map calibrationValue . filter (not . null) . lines

part1 :: InputFileContent -> Int
part1 f = sum (calibrationValues f)


---------- PART TWO ----------

digits :: [String]
digits = 
  [ "zero"
  , "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]

firstLast' :: String -> (Maybe Int, Maybe Int)
firstLast' ""       = (Nothing, Nothing)
firstLast' (c : cs) = (n <|> f, l <|> n)
  where
    (f, l) = firstLast' cs
    n'     = digitToIntMaybe c
    n''    = findIndex (`isPrefixOf` (c : cs)) digits
    n      = n' <|> n''

calibrationValue' :: String -> Int
-- Pre: String contains at least two digits
calibrationValue' s = 10 * f + l
  where (Just f, Just l) = firstLast' s

calibrationValues' :: String -> [Int]
calibrationValues' = map calibrationValue' . filter (not . null) . lines

part2 :: InputFileContent -> Int
part2 f = sum $ calibrationValues' f
