module AdventOfCode.Day5 (day5, part1, part2) where

day5 :: IO ()
day5 = do
  input <- readFile "src/Data/Day5.txt"
  putStr "Day 5 part 1: "
  print . part1 $ input
  putStr "Day 5 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
