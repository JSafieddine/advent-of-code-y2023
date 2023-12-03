{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day3 (day3, part1, part2) where

import Data.Char (isDigit)
import Data.Map (Map, empty, insert, member, union)
import Data.Maybe (mapMaybe)

day3 :: IO ()
day3 = do
  input <- readFile "src/Data/Day3.txt"
  putStr "Day 3 part 1: "
  print . part1 $ input
  putStr "Day 3 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = sum . map _number . filter (isPartNumber b schema) $ pns
  where
    (b, pns, schema) = inputParser input

part2 :: String -> Int
part2 input = sum . mapMaybe (gearRatio pns) $ gs
  where
    (_, pns, _) = inputParser input
    gs = inputParser2 input

data PartNumber = PartNumber
  { _number :: Int,
    _row :: Int,
    _colStart :: Int,
    _colEnd :: Int
  }
  deriving (Show)

type Schematic = Map (Int, Int) Bool

type Bounds = (Int, Int, Int, Int)

type Gear = (Int, Int)

inputParser :: String -> (Bounds, [PartNumber], Schematic)
inputParser input = concatParsedLines (0, 0, length l - 1, length (snd . head $ l) - 1) . map (uncurry parseLine) $ l
  where
    l = zip [0 ..] . map (zip [0 ..]) . init . lines $ input

parseLine :: Int -> [(Int, Char)] -> ([PartNumber], Schematic)
parseLine _ [] = ([], empty)
parseLine row indexedLine@((col, c) : lineRest)
  | c == '.' = parseLine row lineRest
  | isDigit c = let (num, rest) = span (isDigit . snd) indexedLine in addPartNumber (createPartNumber row num) (parseLine row rest)
  | otherwise = addSymbol (col, row) (parseLine row lineRest)

addPartNumber :: PartNumber -> ([PartNumber], Schematic) -> ([PartNumber], Schematic)
addPartNumber pn (pns, schema) = (pn : pns, schema)

createPartNumber :: Int -> [(Int, Char)] -> PartNumber
createPartNumber row numLines =
  PartNumber
    { _number = read . map snd $ numLines,
      _row = row,
      _colStart = fst . head $ numLines,
      _colEnd = fst . last $ numLines
    }

addSymbol :: (Int, Int) -> ([a], Schematic) -> ([a], Schematic)
addSymbol pos (pns, schema) = (pns, insert pos True schema)

concatParsedLines :: Bounds -> [([a], Schematic)] -> (Bounds, [a], Schematic)
concatParsedLines b line = (b, concatMap fst line, foldl union empty (map snd line))

getAdjacentFields :: Bounds -> PartNumber -> [(Int, Int)]
getAdjacentFields (xMin, yMin, xMax, yMax) pn = concat [before, after, above, below]
  where
    before = [(_colStart pn - 1, _row pn) | _colStart pn > xMin]
    after = [(_colEnd pn + 1, _row pn) | _colEnd pn < xMax]
    above = if _row pn > yMin then map (,_row pn - 1) [(max xMin (pn._colStart - 1)) .. (min xMax (pn._colEnd + 1))] else []
    below = if _row pn < yMax then map (,_row pn + 1) [(max xMin (pn._colStart - 1)) .. (min xMax (pn._colEnd + 1))] else []

isPartNumber :: Bounds -> Schematic -> PartNumber -> Bool
isPartNumber b schema pn = any (`member` schema) adjacent
  where
    adjacent = getAdjacentFields b pn

inputParser2 :: String -> [Gear]
inputParser2 input = concatMap (uncurry parseLine2) l
  where
    l = zip [0 ..] . map (zip [0 ..]) . init . lines $ input

parseLine2 :: Int -> [(Int, Char)] -> [Gear]
parseLine2 _ [] = []
parseLine2 row ((col, c) : lineRest)
  | c == '*' = (col, row) : parseLine2 row lineRest
  | otherwise = parseLine2 row lineRest

isAdjacent :: Gear -> PartNumber -> Bool
isAdjacent (x, y) pn = x >= pn._colStart - 1 && x <= pn._colEnd + 1 && y >= pn._row - 1 && y <= pn._row + 1

gearRatio :: [PartNumber] -> Gear -> Maybe Int
gearRatio pns g
  | length adj == 2 = Just . product . map _number $ adj
  | otherwise = Nothing
  where
    adj = filter (isAdjacent g) pns
