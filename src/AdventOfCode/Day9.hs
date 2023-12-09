module AdventOfCode.Day9 (day9, part1, part2) where

day9 :: IO ()
day9 = do
  input <- readFile "src/Data/Day9.txt"
  putStr "Day 9 part 1: "
  print . part1 $ input
  putStr "Day 9 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = sum . map extrapolateLastValue . inputReverseParser

part2 :: String -> Int
part2 = sum . map extrapolateFirstValue . inputParser

type Report = [Int]

type ReverseReport = [Int]

type DiffReverseReport = [Int]

inputReverseParser :: String -> [ReverseReport]
inputReverseParser = map (reverse . map read . words) . init . lines

inputParser :: String -> [Report]
inputParser = map (map read . words) . init . lines

rdiff :: ReverseReport -> DiffReverseReport
rdiff report = zipWith (-) report (tail report)

rdiffs :: ReverseReport -> [DiffReverseReport]
rdiffs report
  | all (== 0) report = []
  | otherwise = dReport : rdiffs dReport
  where
    dReport = rdiff report

extrapolateLastValue :: ReverseReport -> Int
extrapolateLastValue report = sum . map head $ (report : rdiffs report)

diff :: Report -> Report
diff report = zipWith (-) (tail report) report

diffs :: Report -> [Report]
diffs report
  | all (== 0) report = []
  | otherwise = dReport : diffs dReport
  where
    dReport = diff report

extrapolateFirstValue :: Report -> Int
extrapolateFirstValue report = foldr1 (-) . map head $ (report : diffs report)
