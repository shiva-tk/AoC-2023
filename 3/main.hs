import Data.Array
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs, getProgName)


---------- ARRAY CREATION  -----------

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


---------- PART ONE  -----------

indexedNumbers :: String -> [((Int, Int), Int)]
indexedNumbers s = go 0 s
  where
    go :: Int -> String -> [((Int, Int), Int)]
    go _ "" = []
    go i s
      | null d     = go (j + 1) w
      | otherwise  = ((i', j), n) : go (j + 1) w 
      where (u, v) = break isDigit s
            (d, w) = span isDigit v
            i'     = i + length u
            j      = i' + length d - 1
            n      = read d

partNumbers :: String -> [Int]
partNumbers schematic = ns'
  where 
    acs = makeArray schematic
    (_, (m, n)) = bounds acs

    ls   = lines schematic
    jnss = map indexedNumbers ls
    ijns =  concat $ zipWith (\i jns -> map (\((j, j'), n) -> ((i, j, j'), n)) jns) [0..] jnss
    ns   = filter (\((i, j, j'), _) -> isPartNumber i j j') ijns
    ns'  = map (\((_, _, _), n) -> n) ns

    isPartNumber :: Int -> Int -> Int -> Bool
    isPartNumber i j j' = any (isSymbol . (acs !)) toSearch
      where 
        l = [(i, j - 1) | j > 0]
        r = [(i, j' + 1) | j' < n - 1]
        u = [(i - 1, j'') | i > 0, j'' <- [j .. j']]
        d = [(i + 1, j'') | i < m - 1 , j'' <- [j .. j']]
        ul = [(i - 1, j - 1) | i > 0, j > 0]
        ur = [(i - 1, j' + 1) | i > 0, j' < n - 1]
        dl = [(i + 1, j - 1) | i < m - 1, j > 0]
        dr = [(i + 1, j' + 1) | i < m - 1, j' < n - 1]
        toSearch = concat [l, r, u, d, ul, ur, dl, dr]
        isSymbol c = not (isDigit c) && (c /= '.')

part1 :: IO ()
part1 = do
  schematics <- readFile "input.txt"
  print (sum (partNumbers schematics))


---------- PART TWO ----------

indexedGears :: String -> [Int]
indexedGears s = go 0 s
  where
    go :: Int -> String -> [Int]
    go _ "" = []
    go i ('*' : s) = i : go (i + 1) s
    go i (_ : s) = go (i + 1) s

merge :: [(Int, Int)] -> [(Int, Int)]
merge []            = []
merge (ij : ijs)    = ij : merge (filter (not . nextTo ij) ijs)

nextTo :: (Int, Int) -> (Int, Int) -> Bool
nextTo (i, j) (i', j') = abs (j - j') == 1 && i == i'

gearRatios :: String -> [Int]
gearRatios schematics = mapMaybe (uncurry (gearRatio acs)) ijs
  where 
    acs = makeArray schematics
    (_, (m, n)) = bounds acs

    ls  = lines schematics
    jss = map indexedGears ls
    ijs = concat $ zipWith (\i js -> map (i,) js) [0..] jss

gearRatio :: Array (Int, Int) Char -> Int -> Int -> Maybe Int
gearRatio acs i j
  | length adjacentParts == 2 = Just (product adjacentPartNumbers)
  | otherwise                 = Nothing
  where
    (_, (m, n))         = bounds acs
    u                   = [(i - 1, j) | i > 0]
    d                   = [(i + 1, j) | i < m - 1]
    l                   = [(i, j - 1) | j > 0]
    r                   = [(i, j + 1) | j < n - 1]
    ul                  = [(i - 1, j - 1) | i > 0, j > 0]
    ur                  = [(i - 1, j + 1) | i > 0, j < n - 1]
    dl                  = [(i + 1, j - 1) | i < m - 1, j > 0]
    dr                  = [(i + 1, j + 1) | i < m - 1, j < n - 1]
    adjacents           = concat [u, d, l, r, ul, ur, dl, dr]
    adjacentParts       = merge (filter (isDigit . (acs !)) adjacents)
    adjacentPartNumbers = map (uncurry (numberAt acs)) adjacentParts

numberAt :: Array (Int, Int) Char -> Int -> Int -> Int
numberAt acs i j = go j j
  where
    go :: Int -> Int -> Int
    go j j' 
      | j  - 1 >= 0 && isDigit (acs ! (i, j  - 1)) = go (j - 1) j'
      | j' + 1 <  m && isDigit (acs ! (i, j' + 1)) = go j (j' + 1)
      | otherwise = read s
      where
        s = [acs ! (i, j'') | j'' <- [j .. j']]
        (_, (m, _)) = bounds acs

part2 :: IO ()
part2 = do
  schematics <- readFile "input.txt"
  print (sum (gearRatios schematics))
  

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
