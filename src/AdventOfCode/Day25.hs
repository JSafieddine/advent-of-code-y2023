module AdventOfCode.Day25 (day25, part1, part2) where

day25 :: IO ()
day25 = do
  input <- readFile "src/Data/Day25.txt"
  putStr "Day 25 part 1: "
  print . part1 $ input
  putStr "Day 25 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
