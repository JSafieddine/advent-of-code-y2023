module AdventOfCode.Day13 (day13, part1, part2) where

import Control.Monad.State (MonadState (put), State, evalState, get)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)

day13 :: IO ()
day13 = do
  input <- readFile "src/Data/Day13.txt"
  putStr "Day 13 part 1: "
  print . part1 $ input
  putStr "Day 13 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = sum . map (findReflection isPerfectReflection) . inputParser

part2 :: String -> Int
part2 = sum . map (findReflection isSmudgedReflection) . inputParser

type Pattern = [String]

data PatternState = PatternState {_visited :: Pattern, _restPattern :: Pattern}

inputParser :: String -> [Pattern]
inputParser = splitOn [""] . lines . init

hFindReflection :: (Pattern -> Pattern -> Bool) -> State PatternState (Maybe Int)
hFindReflection isReflection = do
  ps <- get
  case _restPattern ps of
    [] -> return Nothing
    [_] -> return Nothing
    (a : rest) ->
      if isReflection (a : _visited ps) rest
        then return . Just . length $ (a : _visited ps)
        else do
          put ps {_visited = a : _visited ps, _restPattern = rest}
          hFindReflection isReflection

findReflection :: (Pattern -> Pattern -> Bool) -> Pattern -> Int
findReflection isReflection pattern
  | isJust hReflection = 100 * fromJust hReflection
  | isJust vReflection = fromJust vReflection
  | otherwise = error "Pattern has no reflection!"
  where
    hReflection = evalState (hFindReflection isReflection) (initPatternState pattern)
    vReflection = evalState (hFindReflection isReflection) (initPatternState . transpose $ pattern)

initPatternState :: Pattern -> PatternState
initPatternState pattern = PatternState {_visited = [], _restPattern = pattern}

isPerfectReflection :: Pattern -> Pattern -> Bool
isPerfectReflection a = and . zipWith (==) a

isSmudgedReflection :: Pattern -> Pattern -> Bool
isSmudgedReflection a b = 1 == sum (zipWith lineDiff a b)

lineDiff :: String -> String -> Int
lineDiff a = length . filter not . zipWith (==) a
