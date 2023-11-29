module AdventOfCode.Day8 (day8, part1, part2) where

day8 :: IO ()
day8 = do
  input <- readFile "src/Data/Day8.txt"
  putStr "Day 8 part 1: "
  print . part1 $ input
  putStr "Day 8 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
