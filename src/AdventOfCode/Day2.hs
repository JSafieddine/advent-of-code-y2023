module AdventOfCode.Day2 (day2, part1, part2) where

day2 :: IO ()
day2 = do
  input <- readFile "src/Data/Day2.txt"
  putStr "Day 2 part 1: "
  print . part1 $ input
  putStr "Day 2 part 2: "
  print . part2 $ input


part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
