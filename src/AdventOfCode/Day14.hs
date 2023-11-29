module AdventOfCode.Day14 (day14, part1, part2) where

day14 :: IO ()
day14 = do
  input <- readFile "src/Data/Day14.txt"
  putStr "Day 14 part 1: "
  print . part1 $ input
  putStr "Day 14 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
