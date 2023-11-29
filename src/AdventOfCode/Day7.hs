module AdventOfCode.Day7 (day7, part1, part2) where

day7 :: IO ()
day7 = do
  input <- readFile "src/Data/Day7.txt"
  putStr "Day 7 part 1: "
  print . part1 $ input
  putStr "Day 7 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
