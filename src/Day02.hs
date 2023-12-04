module Day02 (solution) where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Lib (Solution, InputFileContent, dropPrefixMaybe, splitOn)

solution :: Solution
solution = (part1, part2)

---------- DATA TYPES ----------

data Colour = Red Int | Green Int | Blue Int
  deriving Show

count :: Colour -> Int
count (Red   n) = n
count (Green n) = n
count (Blue  n) = n

type Information = [Colour]

data Game = Game Int [Information]
  deriving Show

gameId :: Game -> Int
gameId (Game n _) = n


---------- INPUT PARSING ----------

parseColour :: String -> String -> Maybe Colour
parseColour d s = do
  n <- readMaybe d
  case s of
    "red"   -> Just (Red n) 
    "green" -> Just (Green n)
    "blue"  -> Just (Blue n)
    _       -> Nothing

parseInformation :: [String] -> Information
parseInformation []  = []
parseInformation (d : c : ncs) =
  case mc of
    Just c' -> c' : parseInformation ncs
    Nothing -> parseInformation ncs
  where
    mc = parseColour d c

parseGame :: String -> Maybe Game
parseGame s = do
  s'            <- dropPrefixMaybe "Game " s
  let (d, s'') = break (== ':') s'
  n <- readMaybe d
  s'''          <- dropPrefixMaybe ": " s''
  let is        = splitOn ';' s'''
  let is'       = map (splitOn ',') is
  let is''      = map (concatMap words) is'
  let is'''     = map parseInformation is''
  return (Game n is''')

parseGames :: String -> [Game]
parseGames = mapMaybe parseGame . lines


---------- PART ONE ----------

maxRed   = 12
maxGreen = 13
maxBlue  = 14

isPossible :: Game -> Bool
isPossible (Game _ [])       = True
isPossible (Game n (i : is)) = all colourPossible i && isPossible (Game n is)

colourPossible :: Colour -> Bool
colourPossible (Red n)   = n <= maxRed
colourPossible (Green n) = n <= maxGreen
colourPossible (Blue n)  = n <= maxBlue

part1 :: InputFileContent -> Int
part1 f = n
  where
    gs  = parseGames f
    gs' = filter isPossible gs
    ns  = map gameId gs'
    n   = sum ns


---------- PART ONE ----------

minColoursNeeded :: Game -> [Colour]
minColoursNeeded (Game _ [])       = []
minColoursNeeded (Game n (i : is)) = foldr updateMin (minColoursNeeded (Game n is)) i
  where
    updateMin c         []               = [c]
    updateMin (Red m)   ((Red n) : cs)   = Red (max m n) : cs
    updateMin (Green m) ((Green n) : cs) = Green (max m n) : cs
    updateMin (Blue m)  ((Blue n) : cs)  = Blue (max m n) : cs
    updateMin c'         (c : cs)        = c : updateMin c' cs

power :: [Colour] -> Int
power = product . map count

part2 :: InputFileContent -> Int 
part2 f = sum ps
  where
    gs = parseGames f
    cs = map minColoursNeeded gs
    ps = map power cs
