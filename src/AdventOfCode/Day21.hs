module AdventOfCode.Day21 (day21, part1, part2) where

day21 :: IO ()
day21 = do
  input <- readFile "src/Data/Day21.txt"
  putStr "Day 21 part 1: "
  print . part1 $ input
  putStr "Day 21 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
