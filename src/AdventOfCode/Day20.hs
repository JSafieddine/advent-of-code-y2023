module AdventOfCode.Day20 (day20, part1, part2) where

day20 :: IO ()
day20 = do
  input <- readFile "src/Data/Day20.txt"
  putStr "Day 20 part 1: "
  print . part1 $ input
  putStr "Day 20 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
