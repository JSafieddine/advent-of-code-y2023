module AdventOfCode.Day18 (day18, part1, part2) where

day18 :: IO ()
day18 = do
  input <- readFile "src/Data/Day18.txt"
  putStr "Day 18 part 1: "
  print . part1 $ input
  putStr "Day 18 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
