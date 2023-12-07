{-# LANGUAGE GADTs #-}

module AdventOfCode.Day7 (day7, part1, part2) where

import Data.List (group, singleton, sort, sortOn)

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord)

newtype JCard = JCard Card deriving (Show, Eq)

instance Ord JCard where
  (JCard Jack) `compare` (JCard Jack) = EQ
  (JCard Jack) `compare` _ = LT
  _ `compare` (JCard Jack) = GT
  (JCard c1) `compare` (JCard c2) = c1 `compare` c2

data HandType
  = HighCard
  | Pair
  | DoublePair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

data Hand where
  Hand :: {_cards :: [Card]} -> Hand
  deriving (Show, Eq)

newtype JHand = JHand Hand deriving (Show, Eq)

instance Ord Hand where
  h1 `compare` h2 = case handType h1 `compare` handType h2 of
    EQ -> _cards h1 `compare` _cards h2
    o -> o

handType :: Hand -> HandType
handType hand
  | length g == 5 = HighCard
  | length g == 4 = Pair
  | length g == 3 && maximum gl == 2 = DoublePair
  | length g == 3 && maximum gl == 3 = ThreeOfAKind
  | length g == 2 && maximum gl == 3 = FullHouse
  | length g == 2 && maximum gl == 4 = FourOfAKind
  | length g == 1 = FiveOfAKind
  | otherwise = error "Invalid Hand!"
  where
    g = group . sort . _cards $ hand
    gl = sort . map length $ g

handTypeJ :: JHand -> HandType
handTypeJ = handType . convertJoker

convertJoker :: JHand -> Hand
convertJoker (JHand hand)
  | all (== Jack) (_cards hand) = hand
  | Jack `elem` _cards hand = Hand . map replace . _cards $ hand
  | otherwise = hand
  where
    bestCard = head . last . sortOn length . group . sort . filter (/= Jack) . _cards $ hand
    replace c = if c == Jack then bestCard else c

instance Ord JHand where
  jh1 `compare` jh2 = case handTypeJ jh1 `compare` handTypeJ jh2 of
    EQ -> getJCards jh1 `compare` getJCards jh2
    o -> o

getJCards :: JHand -> [JCard]
getJCards (JHand hand) = map JCard . _cards $ hand

day7 :: IO ()
day7 = do
  input <- readFile "src/Data/Day7.txt"
  putStr "Day 7 part 1: "
  print . part1 $ input
  putStr "Day 7 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = sum . zipWith (curry winnings) [1 ..] . sortOn fst . inputParser
  where
    winnings (rank, (_, bid)) = rank * bid

part2 :: String -> Int
part2 = sum . zipWith (curry winnings) [1 ..] . sortOn (JHand . fst) . inputParser
  where
    winnings (rank, (_, bid)) = rank * bid

inputParser :: String -> [(Hand, Int)]
inputParser = map (lineParser . words) . init . lines

lineParser :: [String] -> (Hand, Int)
lineParser [hand, bid] = (read hand, read bid)
lineParser _ = error "Invalid line!"

instance Read Card where
  readsPrec _ (c : rest)
    | c == '2' = [(Two, rest)]
    | c == '3' = [(Three, rest)]
    | c == '4' = [(Four, rest)]
    | c == '5' = [(Five, rest)]
    | c == '6' = [(Six, rest)]
    | c == '7' = [(Seven, rest)]
    | c == '8' = [(Eight, rest)]
    | c == '9' = [(Nine, rest)]
    | c == 'T' = [(Ten, rest)]
    | c == 'J' = [(Jack, rest)]
    | c == 'Q' = [(Queen, rest)]
    | c == 'K' = [(King, rest)]
    | c == 'A' = [(Ace, rest)]
    | otherwise = []
  readsPrec _ _ = []

instance Read Hand where
  readsPrec _ hand = [(Hand . map (read . singleton) $ h, r)]
    where
      (h, r) = span (`elem` "23456789TJQKA") hand
