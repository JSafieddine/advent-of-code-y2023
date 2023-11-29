module AdventOfCode.Day1 (day1, part1, part2) where

day1 :: IO ()
day1 = do
  input <- readFile "src/Data/Day1.txt"
  putStr "Day 1 part 1: "
  print . part1 $ input
  putStr "Day 1 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
