{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AdventOfCode.Day12 where

import Data.Functor (($>))
import Data.List (group, intercalate)
import Data.List.Split (dropBlanks, dropDelims, split, whenElt)
import Data.Tuple (swap)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, parse, sepBy, some, (<|>))
import Text.Megaparsec.Char (char, eol, hspace)
import Text.Megaparsec.Char.Lexer (decimal)

day12 :: IO ()
day12 = do
  input <- readFile "src/Data/Day12.txt"
  putStr "Day 12 part 1: "
  print . part1 $ input
  putStr "Day 12 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = sum . map (uncurry countArangements . swap) $ springs
  where
    springs = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Can't parse input!"

part2 :: String -> Int
part2 input = sum . map ((uncurry countArangements . swap) . unfold) $ springs
  where
    springs = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Can't parse input!"

type Parser = Parsec Void String

data SpringState = Operational | Broken | Unknown deriving (Show, Eq)

type SpringLine = ([SpringState], [Int])

springParser :: Parser SpringState
springParser = char '.' $> Operational <|> char '#' $> Broken <|> char '?' $> Unknown

springLineParser :: Parser [SpringState]
springLineParser = some springParser

lineParser :: Parser SpringLine
lineParser = (,) <$> springLineParser <* hspace <*> decimal `sepBy` char ',' <* eol

inputParser :: Parser [SpringLine]
inputParser = some lineParser <* eol <* eof

substitueUnknown :: [SpringState] -> [[SpringState]]
substitueUnknown [] = []
substitueUnknown [spring]
  | spring == Unknown = [[Operational], [Broken]]
  | otherwise = [[spring]]
substitueUnknown (spring : rest)
  | spring == Unknown = substitueUnknown (Operational : rest) ++ substitueUnknown (Broken : rest)
  | otherwise = map (spring :) . substitueUnknown $ rest

isValid :: [Int] -> [SpringState] -> Bool
isValid broken springs = brokenSprings == broken
  where
    brokenSprings = map snd . filter ((== Broken) . fst) . groupCount $ springs

groupCount :: [SpringState] -> [(SpringState, Int)]
groupCount = undefined --  map (head . length) . group

countArangements :: [Int] -> [SpringState] -> Int
countArangements broken = length . filter (isValid broken) . substitueUnknown

unfold :: ([SpringState], [Int]) -> ([SpringState], [Int])
unfold (springs, broken) = (intercalate [Unknown] . replicate 5 $ springs, concat . replicate 5 $ broken)

splitOnOperational :: [SpringState] -> [[SpringState]]
splitOnOperational = split (dropBlanks . dropDelims . whenElt $ (== Operational))

splitOnBroken :: [SpringState] -> [[SpringState]]
splitOnBroken = split (dropBlanks . whenElt $ (== Broken))

countSubListARangements :: (Int, [[SpringState]], [Int]) -> [(Int, [[SpringState]], [Int])]
countSubListARangements (n, [], []) = [(n, [], [])] -- we consumed all springs and distributed all broken springs -> positive end of recursion
countSubListARangements (_, [], _) = [(0, [], [])] -- we consumed all springs but some broken springs where not consumed -> negative end of recursion
countSubListARangements (n, springs, [])
  | any (elem Broken) springs = [(0, [], [])]
  | otherwise = [(n, [], [])]
countSubListARangements (n, spring : springs, broken : brokens)
  | Broken `elem` spring && length spring < broken = [(0, [], [])] -- We need to consume this broken spring here but can't
  | Broken `elem` spring && length spring == broken = countSubListARangements (n, springs, brokens) -- we need to consume the broken spring here and we consume the whole spring
  | Broken `notElem` spring && length spring == broken = countSubListARangements (2 * n, springs, broken : brokens) ++ countSubListARangements (2 * n, springs, brokens) -- we can consume the broken spring here or consume operational springs
  | length spring > broken = undefined
  where
    parts = splitOnBroken spring
