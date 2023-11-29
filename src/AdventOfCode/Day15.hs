module AdventOfCode.Day15 (day15, part1, part2) where

day15 :: IO ()
day15 = do
  input <- readFile "src/Data/Day15.txt"
  putStr "Day 15 part 1: "
  print . part1 $ input
  putStr "Day 15 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
