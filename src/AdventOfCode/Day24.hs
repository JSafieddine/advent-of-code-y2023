module AdventOfCode.Day24 (day24, part1, part2) where

day24 :: IO ()
day24 = do
  input <- readFile "src/Data/Day24.txt"
  putStr "Day 24 part 1: "
  print . part1 $ input
  putStr "Day 24 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
