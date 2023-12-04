module Day04 (solution) where

import Lib (dropPrefixMaybe, for, splitOn, InputFileContent, Solution, traceMaybe)
import Data.Char (isDigit, isSpace)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

solution :: Solution
solution = (part1, part2)

data ScratchCard = ScratchCard Int IntSet [Int]
  deriving (Show)


---------- PART ONE ----------

parseScratchCard :: String -> Maybe ScratchCard
parseScratchCard s = do
  s' <- dropPrefixMaybe "Card" s
  let (ds, s'') = (span isDigit . dropWhile isSpace) s'
  n <- readMaybe ds
  s''' <- (dropPrefixMaybe ":" . dropWhile isSpace) s''
  let (u, v) = (break (== '|') . dropWhile isSpace) s'''
  v' <- dropPrefixMaybe "|" v
  ws <- mapM readMaybe (words u)
  ns <- mapM readMaybe (words v')
  return $ ScratchCard n (IntSet.fromList ws) ns

parseScratchCards :: String -> [ScratchCard]
parseScratchCards = mapMaybe parseScratchCard . lines

matches :: ScratchCard -> Int
matches (ScratchCard _ ws ns) = length $ filter (`IntSet.member` ws ) ns

points :: ScratchCard -> Int
points sc
  | m == 0    = 0
  | otherwise = 2 ^ (m - 1)
  where
    m = matches sc
    
part1 :: InputFileContent -> Int
part1 s = sum $ map points cards
  where cards = parseScratchCards s


---------- PART ONE ----------

scratchCardsWon :: [ScratchCard] -> Int
scratchCardsWon scs = go scs (repeat 1)
  where
    go :: [ScratchCard] -> [Int] -> Int
    go [] _ = 0
    go (sc : scs) (n : ns) = n + go scs ns'
      where ns' = for (matches sc) (+ n) ns

part2 :: InputFileContent -> Int
part2 = scratchCardsWon . parseScratchCards
