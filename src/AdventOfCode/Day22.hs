module AdventOfCode.Day22 (day22, part1, part2) where

day22 :: IO ()
day22 = do
  input <- readFile "src/Data/Day22.txt"
  putStr "Day 22 part 1: "
  print . part1 $ input
  putStr "Day 22 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 _ = undefined

part2 :: String -> Int
part2 _ = undefined
