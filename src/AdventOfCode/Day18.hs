{-# LANGUAGE ScopedTypeVariables #-}

module AdventOfCode.Day18 (day18, part1, part2) where

import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, between, count, parse, some, try, (<|>))
import Text.Megaparsec.Char (char, eol, hspace, printChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (Left, Right, lookup)
import qualified Prelude (Either (Left, Right))

day18 :: IO ()
day18 = do
  input <- readFile "src/Data/Day18.txt"
  putStr "Day 18 part 1: "
  print . part1 $ input
  putStr "Day 18 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = picksPointCount . circumfencePolygon $ digs
  where
    digs = case parse inputParser "" input of
      (Prelude.Right result) -> result
      (Prelude.Left _) -> error "Can't parse input!"

part2 :: String -> Int
part2 input = picksPointCount . circumfencePolygon . map convertDig $ digs
  where
    digs = case parse inputParser "" input of
      (Prelude.Right result) -> result
      (Prelude.Left _) -> error "Can't parse input!"

data Dig
  = Up {_amount :: Int, _color :: String}
  | Down {_amount :: Int, _color :: String}
  | Left {_amount :: Int, _color :: String}
  | Right {_amount :: Int, _color :: String}
  deriving (Show)

type Parser = Parsec Void String

digParser :: Char -> (Int -> String -> Dig) -> Parser Dig
digParser dir dig =
  dig
    <$> (char dir *> hspace *> decimal <* hspace)
    <*> between (char '(') (char ')') (count 7 printChar)
    <* eol

lineParser :: Parser Dig
lineParser =
  try (digParser 'U' Up)
    <|> try (digParser 'D' Down)
    <|> try (digParser 'L' Left)
    <|> try (digParser 'R' Right)

inputParser :: Parser [Dig]
inputParser = some lineParser <* eol <* eof

circumfencePolygon' :: [Dig] -> (Int, Int) -> [(Int, Int)]
circumfencePolygon' [] cur = [cur]
circumfencePolygon' (dig : digs) cur@(x, y) = cur : circumfencePolygon' digs next
  where
    next = case dig of
      (Up n _) -> (x, y - n)
      (Down n _) -> (x, y + n)
      (Left n _) -> (x - n, y)
      (Right n _) -> (x + n, y)

circumfencePolygon :: [Dig] -> [(Int, Int)]
circumfencePolygon digs = circumfencePolygon' digs (0, 0)

type Point = (Int, Int)

type Polygon = [Point]

convertDig :: Dig -> Dig
convertDig dig = case read [last . _color $ dig] of
  (0 :: Int) -> Right amount color
  1 -> Down amount color
  2 -> Left amount color
  3 -> Up amount color
  _ -> error "Invalid conversion!"
  where
    amount = read ("0x" ++ (take 5 . tail . _color $ dig))
    color = _color dig

-- returns double the area
shoelace :: Polygon -> Int
shoelace polygon = forward - backward
  where
    xs = map fst polygon
    ys = map snd polygon
    forward = sum . zipWith (*) xs . tail $ ys
    backward = sum . zipWith (*) (tail xs) $ ys

-- A = i + b/2 - 1 => i = A - b/2 + 1 => i+b = A + b/2 + 1
picksPointCount :: Polygon -> Int
picksPointCount polygon = (area + boundaryPointCount polygon) `div` 2 + 1
  where
    area = shoelace polygon

boundaryPointCount :: Polygon -> Int
boundaryPointCount polygon = sum . zipWith dist polygon $ tail polygon

-- we can use the manhattan norm here as all edges are vertical or horizontal
dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
