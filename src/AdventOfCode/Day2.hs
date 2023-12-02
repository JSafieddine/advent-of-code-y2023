module AdventOfCode.Day2 (day2, part1, part2, drawParser) where

import Control.Monad.Permutations (intercalateEffect, toPermutationWithDefault)
import Data.Map (Map, fromList, unionWith, (!))
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, parse, sepBy1, some, try)
import Text.Megaparsec.Char (eol, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

type Draw = Map String Int

data Game = Game {_Number :: Int, _Draws :: [Draw]}
  deriving (Show)

day2 :: IO ()
day2 = do
  input <- readFile "src/Data/Day2.txt"
  putStr "Day 2 part 1: "
  print . part1 $ input
  putStr "Day 2 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = sum . map _Number . filter possibleGame $ games
  where
    games = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

part2 :: String -> Int
part2 input = sum . map (product . minimumCubes) $ games
  where
    games = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

colorParser :: String -> Parser (String, Int)
colorParser col = do
  nr <- decimal
  _ <- spaceChar
  color <- string col
  return (color, nr)

redParser :: Parser (String, Int)
redParser = try . colorParser $ "red"

greenParser :: Parser (String, Int)
greenParser = try . colorParser $ "green"

blueParser :: Parser (String, Int)
blueParser = try . colorParser $ "blue"

drawParser :: Parser Draw
drawParser = do
  (red, green, blue) <-
    intercalateEffect (string ", ") $
      (,,)
        <$> toPermutationWithDefault ("red", 0) redParser
        <*> toPermutationWithDefault ("green", 0) greenParser
        <*> toPermutationWithDefault ("blue", 0) blueParser
  return . fromList $ [red, green, blue]

gameParser :: Parser Game
gameParser =
  Game
    <$> (string "Game " *> decimal <* string ": ")
    <*> sepBy1 drawParser (string "; ")
    <* eol

inputParser :: Parser [Game]
inputParser = some gameParser <* eol <* eof

possibleDraw :: Draw -> Bool
possibleDraw draw = draw ! "red" < 13 && draw ! "green" < 14 && draw ! "blue" < 15

possibleGame :: Game -> Bool
possibleGame game = all possibleDraw (_Draws game)

minimumCubes :: Game -> Draw
minimumCubes game = foldl (unionWith max) (fromList [("red", 0), ("green", 0), ("blue", 0)]) (_Draws game)
