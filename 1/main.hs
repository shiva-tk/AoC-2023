import GHC.Base  -- <|> (a.k.a mplus)
import Data.Char (isDigit, digitToInt)
import Data.List (elemIndex, findIndex, isPrefixOf)
import System.Environment (getArgs, getProgName)


---------- PART ONE ----------

digitToInt' :: Char -> Maybe Int
digitToInt' c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

firstLast :: String -> (Maybe Int, Maybe Int)
firstLast ""       = (Nothing, Nothing)
firstLast (c : cs) = (n <|> f, l <|> n)
  where
    (f, l) = firstLast cs
    n      = digitToInt' c

calibrationValue :: String -> Int
-- Pre: String contains at least two digits
calibrationValue s = 10 * f + l
  where (Just f, Just l) = firstLast s

calibrationValues :: String -> [Int]
calibrationValues = map calibrationValue . filter (not . null) . lines

part1 :: IO ()
part1 = do
  putStr "Part 1 Solution: "
  calibrationDocument <- readFile "input.txt"
  print $ sum $ calibrationValues calibrationDocument


---------- PART ONE ----------

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
    n'     = digitToInt' c
    n''    = findIndex (`isPrefixOf` (c : cs)) digits
    n      = n' <|> n''

calibrationValue' :: String -> Int
-- Pre: String contains at least two digits
calibrationValue' s = 10 * f + l
  where (Just f, Just l) = firstLast' s

calibrationValues' :: String -> [Int]
calibrationValues' = map calibrationValue' . filter (not . null) . lines

part2 :: IO ()
part2 = do
  putStr "Part 2 Solution: "
  calibrationDocument <- readFile "input.txt"
  print $ sum $ calibrationValues' calibrationDocument


---------- MAIN ----------

data Part = One | Two

parseArgs :: [String] -> Maybe Part
parseArgs ["1"] = Just One
parseArgs ["2"] = Just Two
parseArgs _     = Nothing

printUsage :: IO ()
printUsage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <part_number>"

main :: IO ()
main = do
  args <- getArgs
  let part  = parseArgs args
  case part of
    Just One -> part1
    Just Two -> part2
    Nothing  -> printUsage
