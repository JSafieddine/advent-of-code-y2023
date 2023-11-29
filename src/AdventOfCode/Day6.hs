module AdventOfCode.Day6 (day6, part1, part2) where

day6 :: IO ()
day6 = do
  input <- readFile "src/Data/Day6.txt"
  putStr "Day 6 part 1: "
  print . part1 $ input
  putStr "Day 6 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
