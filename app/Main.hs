module Main where

import System.Environment (getArgs, getProgName)

import Lib   (Solution)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08

daySolutions :: [Solution]
daySolutions = [ Day01.solution
               , Day02.solution
               , Day03.solution
               , Day04.solution
               , Day05.solution
               , Day06.solution
               , Day07.solution
               , Day08.solution
               ]

printUsage :: IO ()
printUsage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <day>" ++ " <part>" ++ " <input_filepath>"
  putStrLn $ "  where <day>  is an integer in the range [1, " ++ show (length daySolutions) ++ "]"
  putStrLn   "        <part> is either 1 or 2"
  putStrLn   "        <input_filepath> is the path to the input file"

main :: IO ()
main = do
  args <- getArgs

  case args of
    [dayStr, partStr, inputPath] -> do
      let day  = read dayStr
      let part = if partStr == "1" then fst else snd
      if day < 1 || day > length daySolutions
        then printUsage
        else do
          let solution = daySolutions !! (day - 1)
          input        <- readFile inputPath
          print $ part solution input

    _ -> printUsage
