module AdventOfCode.Day17 (day17, part1, part2) where

day17 :: IO ()
day17 = do
  input <- readFile "src/Data/Day17.txt"
  putStr "Day 17 part 1: "
  print . part1 $ input
  putStr "Day 17 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
