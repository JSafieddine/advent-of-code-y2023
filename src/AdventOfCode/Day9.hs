module AdventOfCode.Day9 (day9, part1, part2) where

day9 :: IO ()
day9 = do
  input <- readFile "src/Data/Day9.txt"
  putStr "Day 9 part 1: "
  print . part1 $ input
  putStr "Day 9 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
