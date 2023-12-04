module Day04 where -- (solution) where

import Lib (for, InputFileContent, Solution)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Functor (($>))
import Text.Parsec (parse, eof, many)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Combinator (sepBy, many1, endBy)

solution :: Solution
solution = (part1, part2)

data ScratchCard = ScratchCard Int IntSet [Int]
  deriving (Show)

spaces :: Parser ()
spaces = many (char ' ') $> ()

card :: Parser ()
card = spaces *> string "Card" *> spaces

number :: Parser Int
number = (read <$> many1 digit) <* spaces

colon :: Parser ()
colon = char ':' *> spaces

numberList :: Parser [Int]
numberList = many1 number

numberLists :: Parser ([Int], [Int])
numberLists = (,) <$> numberList <* char '|' <* spaces  <*> numberList

scratchCard :: Parser ScratchCard
scratchCard = do
  card
  id <- number
  colon
  (ws, ns) <- numberLists
  return $ ScratchCard id (IntSet.fromList ws) ns

scratchCards :: Parser [ScratchCard]
scratchCards = scratchCard `endBy` char '\n' <* eof

matches :: ScratchCard -> Int
matches (ScratchCard _ ws ns) = length $ filter (`IntSet.member` ws ) ns


---------- PART ONE ----------

points :: ScratchCard -> Int
points sc
  | m == 0    = 0
  | otherwise = 2 ^ (m - 1)
  where
    m = matches sc
    
part1 :: InputFileContent -> Int
part1 s
  | Right cards <- p= sum $ map points cards
  | Left err    <- p = error (show err)
  where p = parse scratchCards "" s


---------- PART TWO ----------

scratchCardsWon :: [ScratchCard] -> Int
scratchCardsWon scs = go scs (repeat 1)
  where
    go :: [ScratchCard] -> [Int] -> Int
    go [] _ = 0
    go (sc : scs) (n : ns) = n + go scs ns'
      where ns' = for (matches sc) (+ n) ns

part2 :: InputFileContent -> Int
part2 s
  | Right cards <- p = scratchCardsWon cards
  | Left err    <- p = error $ show err
  where p = parse scratchCards "" s
