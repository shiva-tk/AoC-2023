module Lib where

import Data.Array
import Data.Char (isDigit, digitToInt)

type Solution = (InputFileContent -> Int, InputFileContent -> Int)

type InputFileContent = String

-- Handy functions for input file parsing
--
digitToIntMaybe :: Char -> Maybe Int
digitToIntMaybe c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

dropMaybe :: Int -> [a] -> Maybe [a]
dropMaybe n l
  | length l > n = Just (drop n l)
  | otherwise    = Nothing

dropPrefixMaybe :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefixMaybe [] ys      = Just ys
dropPrefixMaybe (_ : _) [] = Nothing
dropPrefixMaybe (x : xs) (y : ys)
  | x == y    = dropPrefixMaybe xs ys
  | otherwise = Nothing

splitOn :: Eq a => Show a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn d xs = 
  case t of
    []       -> [h]
    (d : t') -> h : splitOn d t'
  where
    (h, t) = break (== d) xs

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count t (x : xs)
  | x == t    = 1 + count t xs
  | otherwise = count t xs

makeArray :: String -> Array (Int, Int) Char
makeArray cs = array bnds ijcs
  where
    css  = lines cs
    m    = length css
    n    = length (head css)
    bnds = ((0, 0), (m, n))
    ijcs = range bnds `zip` cs
