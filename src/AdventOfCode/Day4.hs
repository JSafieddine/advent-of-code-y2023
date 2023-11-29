module AdventOfCode.Day4 (day4, part1, part2) where

day4 :: IO ()
day4 = do
  input <- readFile "src/Data/Day4.txt"
  putStr "Day 4 part 1: "
  print . part1 $ input
  putStr "Day 4 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
