module AdventOfCode.Day19 (day19, part1, part2) where

day19 :: IO ()
day19 = do
  input <- readFile "src/Data/Day19.txt"
  putStr "Day 19 part 1: "
  print . part1 $ input
  putStr "Day 19 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
