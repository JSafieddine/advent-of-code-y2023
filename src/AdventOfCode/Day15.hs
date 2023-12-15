{-# OPTIONS_GHC -Wno-missing-fields #-}

module AdventOfCode.Day15 (day15, part1, part2) where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Map (Map, adjust, empty, findWithDefault, foldrWithKey, insertWith)

day15 :: IO ()
day15 = do
  input <- readFile "src/Data/Day15.txt"
  putStr "Day 15 part 1: "
  print . part1 $ input
  putStr "Day 15 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = sum . map hash . inputParser

part2 :: String -> Int
part2 = foldrWithKey calcFocusPower 0 . fillBoxes . map commandParser . inputParser

inputParser :: String -> [String]
inputParser = splitOn "," . init

addCharHash :: Int -> Char -> Int
addCharHash curHash c = (curHash + ord c) * 17 `mod` 256

hash :: String -> Int
hash = foldl addCharHash 0

data Command
  = Add {_clabel :: String, _cfocus :: Int}
  | Remove {_clabel :: String}
  deriving (Show)

data Lens = Lens {_label :: String, _focus :: Int} deriving (Show)

instance Eq Lens where
  l1 == l2 = _label l1 == _label l2

commandParser :: String -> Command
commandParser cmd
  | '=' `elem` cmd = case splitOn "=" cmd of
      [label, focus] -> Add label (read focus)
      _ -> error "Invalid add command!"
  | '-' `elem` cmd = Remove (init cmd)
  | otherwise = error "Invalid command!"

type Boxes = Map Int [Lens]

runCommand :: Boxes -> Command -> Boxes
runCommand boxes (Remove label) = adjust (filter (/= Lens {_label = label})) (hash label) boxes
runCommand boxes (Add label focus)
  | newLens `elem` box = adjust (replaceLens newLens) (hash label) boxes
  | otherwise = insertWith (flip (++)) (hash label) [newLens] boxes
  where
    newLens = Lens {_label = label, _focus = focus}
    box = findWithDefault [] (hash label) boxes

replaceLens :: Lens -> [Lens] -> [Lens]
replaceLens newLens = map (\lens -> if lens == newLens then newLens else lens)

fillBoxes :: [Command] -> Boxes
fillBoxes = foldl runCommand empty

calcFocusPower :: Int -> [Lens] -> Int -> Int
calcFocusPower bHash lenses curFocusPower =
  curFocusPower
    + sum (zipWith (\slot lens -> (bHash + 1) * slot * _focus lens) [1 ..] lenses)
