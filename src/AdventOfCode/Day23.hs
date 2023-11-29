module AdventOfCode.Day23 (day23, part1, part2) where

day23 :: IO ()
day23 = do
  input <- readFile "src/Data/Day23.txt"
  putStr "Day 23 part 1: "
  print . part1 $ input
  putStr "Day 23 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
