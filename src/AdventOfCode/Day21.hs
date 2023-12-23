module AdventOfCode.Day21 where

import Data.Map (Map, keys, lookup)
import qualified Data.Map as Map (filter, fromList)
import Data.Set (Set, singleton, size, unions)
import qualified Data.Set as Set (filter, fromList, map)
import Prelude hiding (lookup)

day21 :: IO ()
day21 = do
  input <- readFile "src/Data/Day21.txt"
  putStr "Day 21 part 1: "
  print . part1 $ input
  putStr "Day 21 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = size (iterate (step gm) (singleton . getStartPosition $ gm) !! 64)
  where
    (gm, _) = inputParser input

part2 :: String -> Int
part2 input = size (iterate (step2 gm bounds) (singleton . getStartPosition $ gm) !! 26501365)
  where
    (gm, bounds) = inputParser input

type GardenMap = Map Position Char

type Position = (Int, Int)

type Bounds = (Int, Int)

type StepMap = Map Position Int

inputParser :: String -> (GardenMap, Bounds)
inputParser input =
  ( Map.fromList . concatMap convert . zip [0 ..] . map (zip [0 ..]) $ grid,
    (length . head $ grid, length grid)
  )
  where
    grid = init . lines $ input
    convert (y, xs) = map (\(x, c) -> ((x, y), c)) xs

getAdjacentFields :: (Position -> Bool) -> Position -> Set Position
getAdjacentFields reachable (x, y) = Set.filter reachable . Set.fromList $ [(x - 1, y), (x, y + 1), (x + 1, y), (x, y - 1)]

isReachable :: GardenMap -> Position -> Bool
isReachable gm pos = case lookup pos gm of
  Nothing -> False
  (Just c) -> c /= '#'

getStartPosition :: GardenMap -> Position
getStartPosition = head . keys . Map.filter (== 'S')

step :: GardenMap -> Set Position -> Set Position
step gm = unions . Set.map (getAdjacentFields (isReachable gm))

reachableAfterNSteps :: GardenMap -> Int -> Int
reachableAfterNSteps gm n = size (iterate (step gm) (singleton . getStartPosition $ gm) !! n)

isReachable2 :: GardenMap -> Bounds -> Position -> Bool
isReachable2 gm bounds = isReachable gm . adjustPosition bounds

adjustPosition :: Bounds -> Position -> Position
adjustPosition (xBound, yBound) (x, y) = (x `mod` xBound, y `mod` yBound)

step2 :: GardenMap -> Bounds -> Set Position -> Set Position
step2 gm bounds = unions . Set.map (getAdjacentFields (isReachable2 gm bounds))

steps :: GardenMap -> Position -> [Int]
steps gm = map size . iterate (step gm) . singleton

stepsToLoop' :: [Int] -> Int
stepsToLoop' (x : y : z : rest)
  | x == z = 0
  | otherwise = 1 + stepsToLoop' (y : z : rest)
stepsToLoop' _ = 0

stepsToLoop :: GardenMap -> Position -> Int
stepsToLoop gm = stepsToLoop' . steps gm
