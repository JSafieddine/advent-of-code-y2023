module AdventOfCode.Day11 (day11, part1, part2) where

day11 :: IO ()
day11 = do
  input <- readFile "src/Data/Day11.txt"
  putStr "Day 11 part 1: "
  print . part1 $ input
  putStr "Day 11 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
