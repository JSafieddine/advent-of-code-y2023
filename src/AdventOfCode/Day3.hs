module AdventOfCode.Day3 (day3, part1, part2) where

day3 :: IO ()
day3 = do
  input <- readFile "src/Data/Day3.txt"
  putStr "Day 3 part 1: "
  print . part1 $ input
  putStr "Day 3 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
