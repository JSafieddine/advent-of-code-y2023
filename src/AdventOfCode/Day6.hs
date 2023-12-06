module AdventOfCode.Day6 (day6, part1, part2) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, parse, some)
import Text.Megaparsec.Char (eol, hspace, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

day6 :: IO ()
day6 = do
  input <- readFile "src/Data/Day6.txt"
  putStr "Day 6 part 1: "
  print . part1 $ input
  putStr "Day 6 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = product . map waysToWin $ races
  where
    races = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Can't parse input!"

part2 :: String -> Int
part2 input = waysToWin . getRaceTime $ races
  where
    races = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Can't parse input!"

inputParser :: Parser [(Int, Int)]
inputParser = do
  _ <- string "Time:"
  times <- some (hspace *> decimal)
  _ <- eol
  _ <- string "Distance:"
  distance <- some (hspace *> decimal)
  _ <- eol *> eol *> eof
  return $ zip times distance

waysToWin :: (Int, Int) -> Int
waysToWin (t, d) = length . filter (recordBeat t d) $ [0 .. t]

recordBeat :: Int -> Int -> Int -> Bool
recordBeat t d b = b * (t - b) > d

getRaceTime :: [(Int, Int)] -> (Int, Int)
getRaceTime parts = (convert fst parts, convert snd parts)
  where
    convert part = read . concatMap (show . part)
