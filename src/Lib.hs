module Lib where

import Data.Array
import Data.Char (isDigit, digitToInt)
import Debug.Trace

type Solution = (InputFileContent -> Int, InputFileContent -> Int)

type InputFileContent = String

-- Handy functions for input file parsing

digitToIntMaybe :: Char -> Maybe Int
digitToIntMaybe c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

dropMaybe :: Show a => Int -> [a] -> Maybe [a]
dropMaybe n l
  | length l > n = Just (drop n l)
  | otherwise    = Nothing

dropPrefixMaybe :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefixMaybe [] ys      = Just ys
dropPrefixMaybe (_ : _) [] = Nothing
dropPrefixMaybe (x : xs) (y : ys)
  | x == y    = dropPrefixMaybe xs ys
  | otherwise = Nothing

traceMaybe :: String -> Maybe a -> Maybe a
traceMaybe s Nothing  = trace s Nothing
traceMaybe _ x = x

splitOn :: Eq a => Show a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn d xs = 
  case t of
    []       -> [h]
    (d : t') -> h : splitOn d t'
  where
    (h, t) = break (== d) xs

-- Useful list manipulation functions

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count t (x : xs)
  | x == t    = 1 + count t xs
  | otherwise = count t xs

for :: Int -> (a -> a) -> [a] -> [a]
for 0 _ xs = xs
for n f (x : xs) = f x : for (n - 1) f xs

-- Array utilities

makeArray :: String -> Array (Int, Int) Char
makeArray cs = array bnds ijcs
  where
    css  = lines cs
    m    = length css
    n    = length (head css)
    bnds = ((0, 0), (m, n))
    ijcs = range bnds `zip` cs
