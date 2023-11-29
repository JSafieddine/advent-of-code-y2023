module AdventOfCode.Day13 (day13, part1, part2) where

day13 :: IO ()
day13 = do
  input <- readFile "src/Data/Day13.txt"
  putStr "Day 13 part 1: "
  print . part1 $ input
  putStr "Day 13 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
