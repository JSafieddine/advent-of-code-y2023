module AdventOfCode.Day11 (day11, part1, part2) where

import Data.List (transpose)

day11 :: IO ()
day11 = do
  input <- readFile "src/Data/Day11.txt"
  putStr "Day 11 part 1: "
  print . part1 $ input
  putStr "Day 11 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = sum . map manhattanNorm . pairGalaxies . galaxyPositions . expandUniverse . inputParser

part2 :: String -> Int
part2 = expandedDistances 1000000 . inputParser

type Universe = [String]

inputParser :: String -> Universe
inputParser = init . lines

hExpandUniverse :: Universe -> Universe
hExpandUniverse = foldr expandRow []
  where
    expandRow row universe
      | all (== '.') row = row : row : universe
      | otherwise = row : universe

vExpandUniverse :: Universe -> Universe
vExpandUniverse = transpose . hExpandUniverse . transpose

expandUniverse :: Universe -> Universe
expandUniverse = vExpandUniverse . hExpandUniverse

galaxyPositions :: Universe -> [(Int, Int)]
galaxyPositions = concatMap (map snd . filter ((== '#') . fst)) . addCoordinates

addCoordinates :: Universe -> [[(Char, (Int, Int))]]
addCoordinates universe = zipWith combine universe [0 ..]
  where
    combine row yCoord = zipWith (\c xCoord -> (c, (xCoord, yCoord))) row [0 ..]

manhattanNorm :: ((Int, Int), (Int, Int)) -> Int
manhattanNorm ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

-- Pair an element with every member of the input list
pairElement :: a -> [a] -> [(a, a)]
pairElement _ [] = []
pairElement e (x : xs) = (e, x) : pairElement e xs

pairGalaxies :: [a] -> [(a, a)]
pairGalaxies [] = []
pairGalaxies (x : xs) = pairElement x xs ++ pairGalaxies xs

expansionRates :: Universe -> [Int]
expansionRates universe = zipWith (-) eDist oDist
  where
    oDist = map manhattanNorm . pairGalaxies . galaxyPositions $ universe
    eDist = map manhattanNorm . pairGalaxies . galaxyPositions . expandUniverse $ universe

expandedDistances :: Int -> Universe -> Int
expandedDistances eFactor universe = sum . zipWith (+) oDist . map (* (eFactor - 1)) $ eRates
  where
    eRates = expansionRates universe
    oDist = map manhattanNorm . pairGalaxies . galaxyPositions $ universe
