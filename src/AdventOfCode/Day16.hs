module AdventOfCode.Day16 (day16, part1, part2) where

day16 :: IO ()
day16 = do
  input <- readFile "src/Data/Day16.txt"
  putStr "Day 16 part 1: "
  print . part1 $ input
  putStr "Day 16 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
