import Data.Char (isDigit, digitToInt)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import System.Environment (getArgs, getProgName)


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

dropMaybe :: Show a => Int -> [a] -> Maybe [a]
dropMaybe n l
  | length l > n = Just (drop n l)
  | otherwise    = Nothing

splitOn :: Eq a => Show a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn d xs = 
  case t of
    []       -> [h]
    (d : t') -> h : splitOn d t'
  where
    (h, t) = break (== d) xs

parseColour :: String -> String -> Maybe Colour
parseColour d s = do
  n :: Int <- readMaybe d
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
  s'            <- dropMaybe (length "Game ") s
  let (d, s'') = break (== ':') s'
  n :: Int      <- readMaybe d
  s'''          <- dropMaybe (length ": ") s''
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

part1 :: IO () 
part1 = do
  d <- readFile "input.txt"
  let gs  = parseGames d
  let gs' = filter isPossible gs
  print (sum (map gameId gs'))


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

part2 :: IO () 
part2 = do
  d <- readFile "input.txt"
  let gs  = parseGames d
  let css = map minColoursNeeded gs
  let ps  = map power css
  print $ sum ps


---------- MAIN ----------

data Part = One | Two

parseArgs :: [String] -> Maybe Part
parseArgs ["1"] = Just One
parseArgs ["2"] = Just Two
parseArgs _     = Nothing

printUsage :: IO ()
printUsage = do
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <part_number>")

main :: IO ()
main = do
  args <- getArgs
  let part  = parseArgs args
  case part of
    Just One -> part1
    Just Two -> part2
    Nothing  -> printUsage
