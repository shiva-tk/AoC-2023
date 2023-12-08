module Day08 where

import qualified Deque.Lazy as D
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Maybe (mapMaybe)
import Lib (InputFileContent, Solution, dropPrefixMaybe, takeMaybe)
import qualified Control.Applicative as Set

solution :: Solution
solution = (part1, part2)

type NodeId = String
type Path   = String
type Tree   = Map.HashMap NodeId (NodeId, NodeId)

parseNode :: String -> Maybe (NodeId, NodeId, NodeId)
parseNode s = do
  n <- takeMaybe 3 s
  s' <- dropPrefixMaybe (n ++ " = (") s
  l <- takeMaybe 3 s'
  s'' <- dropPrefixMaybe (l ++ ", ") s'
  r <- takeMaybe 3 s''
  return (n, l, r)

constructTree :: [(NodeId, NodeId, NodeId)] -> Tree
constructTree = Map.fromList . map (\(n, l, r) -> (n, (l, r)))


---------- PART ONE ----------

stepsToFinish :: NodeId -> Path -> Tree -> Int
stepsToFinish n p t = go n (D.fromConsAndSnocLists p []) 0
  where
    go :: NodeId -> D.Deque Char -> Int -> Int
    go "ZZZ" _ s = s
    go n p s
      | d == 'L' = go l p' (s + 1)
      | d == 'R' = go r p' (s + 1)
      where
        Just d = D.head p
        p'     = D.snoc d (D.tail p)
        (l, r) = t Map.! n

part1 :: InputFileContent -> Int
part1 s = stepsToFinish "AAA" p t
  where
    (p : _ : ns) = lines s
    t            = constructTree $ mapMaybe parseNode ns


---------- PART ONE ----------

endsWith :: String -> Char -> Bool
endsWith s c = last s == c

cycleLength :: NodeId -> Path -> Tree -> (Int, Int)
cycleLength n p t = go n (D.fromConsAndSnocLists p []) Map.empty 0
  where
    go :: NodeId -> D.Deque Char -> Map.HashMap NodeId Int -> Int-> (Int, Int)
    go n p v s
      | n `endsWith` 'Z' && 
        Map.member n v = (v Map.! n, s - v Map.! n)
      | d == 'L'       = go l p' v' (s + 1)
      | d == 'R'       = go r p' v' (s + 1)
      where
        Just d = D.head p
        p'     = D.snoc d (D.tail p)
        (l, r) = t Map.! n
        v'     = Map.insert n s v

stepsToFinish' :: [NodeId] -> Path -> Tree -> Int
stepsToFinish' ns p t = go s0 ds
  where
    ocs = map (\n -> cycleLength n p t) ns
    os  = map fst ocs
    cs  = map snd ocs
    s0  = maximum os
    ds  = maximum cs

    go :: Int -> Int -> Int
    go s ds
      | all (==0) [(s - o) `mod` c | (o, c) <- ocs] 
        = s
      | otherwise 
        = go (s + ds) ds

part2 :: InputFileContent -> Int
part2 s = stepsToFinish' ns'' p t
  where
    (p : _ : ns) = lines s
    ns'          = mapMaybe parseNode ns
    ns''         = filter (`endsWith` 'A') $ map (\(n, _, _) -> n) ns'
    t            = constructTree ns'

