module AdventOfCode.Day10 (day10, part1, part2) where

day10 :: IO ()
day10 = do
  input <- readFile "src/Data/Day10.txt"
  putStr "Day 10 part 1: "
  print . part1 $ input
  putStr "Day 10 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
