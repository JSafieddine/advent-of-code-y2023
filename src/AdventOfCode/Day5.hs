module AdventOfCode.Day5 (day5, part1, part2) where

import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, some, someTill)
import Text.Megaparsec.Char (char, eol, hspace, letterChar, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

day5 :: IO ()
day5 = do
  input <- readFile "src/Data/Day5.txt"
  putStr "Day 5 part 1: "
  print . part1 $ input
  putStr "Day 5 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = minimum . map (`getSeedLocation` seedMaps) $ seeds
  where
    (seeds, seedMaps) = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

-- TODO: Refactor data structures to use intervals and map from interval to interval sets instead of expanding seeds!
-- The current implementation takes ages.
part2 :: String -> Int
part2 input = minimum . map (`getSeedLocation` seedMaps) . concatMap expandSeedRange . seedsToSeedRange $ seeds
  where
    (seeds, seedMaps) = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

type Parser = Parsec Void String

data Range = Range {_destination :: Int, _source :: Int, _length :: Int} deriving (Show)

data SeedMap = SeedMap {_sourceType :: String, _destinationType :: String, _ranges :: [Range]} deriving (Show)

data SeedRange = SeedRange {_start :: Int, _seedRangeLength :: Int} deriving (Show)

seedParser :: Parser [Int]
seedParser = string "seeds: " *> some (hspace *> decimal) <* eol

seedMapParser :: Parser SeedMap
seedMapParser = do
  source <- someTill letterChar (char '-')
  _ <- string "to-"
  destination <- someTill letterChar spaceChar
  _ <- string "map:"
  _ <- eol
  ranges <- some rangeParser
  _ <- eol
  return $ SeedMap source destination ranges

rangeParser :: Parser Range
rangeParser = Range <$> decimal <* hspace <*> decimal <* hspace <*> decimal <* eol

inputParser :: Parser ([Int], [SeedMap])
inputParser = do
  seeds <- seedParser
  _ <- eol
  seedMaps <- some seedMapParser
  return (seeds, seedMaps)

getSeedLocation :: Int -> [SeedMap] -> Int
getSeedLocation = foldl mapSeed

mapSeed :: Int -> SeedMap -> Int
mapSeed n m = case mapMaybe (mapRange n) (_ranges m) of
  [] -> n
  (n' : _) -> n'

mapRange :: Int -> Range -> Maybe Int
mapRange n r
  | n >= _source r && n <= _source r + _length r = Just $ _destination r - _source r + n
  | otherwise = Nothing

seedsToSeedRange :: [Int] -> [SeedRange]
seedsToSeedRange [] = []
seedsToSeedRange [_] = error "Odd number of seeds"
seedsToSeedRange (s : l : rest) = SeedRange s l : seedsToSeedRange rest

expandSeedRange :: SeedRange -> [Int]
expandSeedRange sr = [_start sr .. _start sr + _seedRangeLength sr]
