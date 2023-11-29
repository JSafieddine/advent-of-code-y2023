module AdventOfCode.Day12 (day12, part1, part2) where

day12 :: IO ()
day12 = do
  input <- readFile "src/Data/Day12.txt"
  putStr "Day 12 part 1: "
  print . part1 $ input
  putStr "Day 12 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
