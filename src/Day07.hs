module Day07 where

import Data.List (nub, find, sort, sortBy)
import Lib (InputFileContent, Solution, orElse, count)
import Data.Function (on)
import Debug.Trace

solution :: Solution
solution = (part1, part2)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
  deriving (Eq, Ord, Show)

type Hand = (Card, Card, Card, Card, Card)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Ord, Show)


---------- PART ONE ----------

parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = Ten
parseCard 'J' = Jack
parseCard 'Q' = Queen
parseCard 'K' = King
parseCard 'A' = Ace

parseHand :: (Char -> Card) -> String -> Hand
parseHand parseCard (a:b:c:d:e:_) = (parseCard a, parseCard b, parseCard c, parseCard d, parseCard e)

parseBid :: (Char -> Card) -> String -> (Hand, Int)
parseBid parseCard s = (parseHand parseCard h, read b)
  where (h : b : _) = words s

parseBids :: (Char -> Card) -> String -> [(Hand, Int)]
parseBids parseCard = map (parseBid parseCard) . lines

countCards :: [Card] -> [(Card, Int)]
countCards []       = []
countCards (c : cs) = (c, n + 1) : cns'
  where 
    cns    = countCards cs
    cns'   = filter ((/= c) . fst) cns
    (_, n) = find ((== c) . fst) cns `orElse` (c, 0)

handType :: Hand -> HandType
handType (a, b, c, d, e)
  | length cns == 1                   = FiveOfAKind
  | length cns == 2 && isFourOfAKind  = FourOfAKind
  | length cns == 2 && isFullHouse    = FullHouse
  | length cns == 3 && isThreeofAKind = ThreeOfAKind
  | length cns == 3 && isTwoPair      = TwoPair
  | length cns == 4                   = OnePair
  | length cns == 5                   = HighCard
  where 
    cns                         = countCards [a, b, c, d, e]
    [(_, n), (_, n')]           = cns
    isFourOfAKind               = n == 4 || n' == 4
    isFullHouse                 = True
    [(_, n1), (_, n2), (_, n3)] = cns
    isThreeofAKind              = n1 == 3 || n2 == 3 || n3 == 3
    isTwoPair                   = True

part1 :: InputFileContent -> Int
part1 s = sum (zipWith (*) bs' [1..])
  where bs  = parseBids parseCard s
        hs  = map (\(h,b) -> (handType h, h, b)) bs
        hs' = sort hs
        bs' = map (\(_, _, b) -> b) hs'


---------- PART TWO ----------

parseCard' :: Char -> Card
parseCard' 'J' = Joker
parseCard' c   = parseCard c

handType' :: Hand -> HandType
handType' (a, b, c, d, e)
  | Joker `notElem` cs                 = handType (a, b, c, d, e)
  | null cns                           = FiveOfAKind
  | length cns' == 1                   = FiveOfAKind
  | length cns' == 2 && isFourOfAKind  = FourOfAKind
  | length cns' == 2 && isFullHouse    = FullHouse
  | length cns' == 3 && isThreeofAKind = ThreeOfAKind
  | length cns' == 3 && isTwoPair      = TwoPair
  | length cns' == 4                   = OnePair
  where
    cs                          = [a, b, c, d, e]
    cs'                         = filter (/= Joker) cs
    nj                          = count Joker cs
    cns                         = sortBy (compare `on` snd) $ countCards cs'
    cns'                        = init cns ++ [(bc, bn + nj)]
    (bc, bn)                    = last cns
    [(_, n0), (_, n1)]          = cns'
    isFourOfAKind               = n0 == 4 || n1 == 4
    isFullHouse                 = True
    [(_, n2), (_, n3), (_, n4)] = cns'
    isThreeofAKind              = n2 == 3 || n3 == 3 || n4 == 3
    isTwoPair                   = True

part2 :: InputFileContent -> Int
part2 s = sum (zipWith (*) bs' [1..])
  where bs  = parseBids parseCard' s
        hs  = map (\(h,b) -> (handType' h, h, b)) bs
        hs' = sort hs
        bs' = map (\(_, _, b) -> b) hs'
