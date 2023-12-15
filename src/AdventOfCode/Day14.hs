module AdventOfCode.Day14 (day14, part1, part2) where

import Control.Monad.State (MonadState (put), State, evalState, get)
import Data.List (elemIndex, intercalate, sortBy, transpose)
import Data.List.Split (splitOn)
import Prelude hiding (cycle)

day14 :: IO ()
day14 = do
  input <- readFile "src/Data/Day14.txt"
  putStr "Day 14 part 1: "
  print . part1 $ input
  putStr "Day 14 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = calculcateLoad . tiltDirection North . inputParser

part2 :: String -> Int
part2 input = calculcateLoad (iterate cycle loopCycle !! cyclesFromLoop)
  where
    rocks = inputParser input
    (cycleEnd, cycleStart, loopCycle) = evalState findLoop [rocks]
    cyclesFromLoop = (1000000000 - cycleStart) `mod` (cycleEnd - cycleStart)

inputParser :: String -> [String]
inputParser = init . lines

data TiltDirection = North | West | South | East deriving (Show, Enum, Bounded)

tiltDirection :: TiltDirection -> [String] -> [String]
tiltDirection North = transpose . tilt . transpose
tiltDirection West = tilt
tiltDirection South = transpose . map reverse . tilt . map reverse . transpose
tiltDirection East = map reverse . tilt . map reverse

tilt :: [String] -> [String]
tilt = map ((intercalate "#" . map (sortBy (flip compare))) . splitOn "#")

calculcateLoad :: [String] -> Int
calculcateLoad = sum . map calculcateLoadLine . transpose

calculcateLoadLine :: String -> Int
calculcateLoadLine = sum . map fst . filter ((== 'O') . snd) . zip [1 ..] . reverse

cycle :: [String] -> [String]
cycle rocks = foldl (flip tiltDirection) rocks [minBound .. maxBound]

findLoop :: State [[String]] (Int, Int, [String])
findLoop = do
  prevCycles <- get
  let nextCycle = cycle . head $ prevCycles
  case nextCycle `elemIndex` prevCycles of
    (Just n) -> return (length prevCycles, length prevCycles - n - 1, nextCycle)
    Nothing -> do
      put (nextCycle : prevCycles)
      findLoop
