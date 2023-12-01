module AdventOfCode.Day1 (day1, part1, part2) where

import Data.Char (isDigit)

day1 :: IO ()
day1 = do
  input <- readFile "src/Data/Day1.txt"
  putStr "Day 1 part 1: "
  print . part1 $ input
  putStr "Day 1 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = sum . map calibrationValue . init . lines

part2 :: String -> Int
part2 = sum . map (calibrationValue . toDigit) . init . lines

calibrationValue :: String -> Int
calibrationValue l = read [head d, last d]
  where
    d = filter isDigit l

toDigit :: String -> String
toDigit l@('o' : 'n' : 'e' : _) = '1' : (toDigit . tail $ l)
toDigit l@('t' : 'w' : 'o' : _) = '2' : (toDigit . tail $ l)
toDigit l@('t' : 'h' : 'r' : 'e' : 'e' : _) = '3' : (toDigit . tail $ l)
toDigit l@('f' : 'o' : 'u' : 'r' : _) = '4' : (toDigit . tail $ l)
toDigit l@('f' : 'i' : 'v' : 'e' : _) = '5' : (toDigit . tail $ l)
toDigit l@('s' : 'i' : 'x' : _) = '6' : (toDigit . tail $ l)
toDigit l@('s' : 'e' : 'v' : 'e' : 'n' : _) = '7' : (toDigit . tail $ l)
toDigit l@('e' : 'i' : 'g' : 'h' : 't' : _) = '8' : (toDigit . tail $ l)
toDigit l@('n' : 'i' : 'n' : 'e' : _) = '9' : (toDigit . tail $ l)
toDigit (d : r) = d : toDigit r
toDigit [] = []
