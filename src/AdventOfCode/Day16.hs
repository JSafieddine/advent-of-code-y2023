{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day16 (day16, part1, part2) where

import Control.Monad.State (State, evalState, get, put)
import Data.Maybe (isJust)
import Data.Set (Set, empty, filter, fromList, insert, singleton, size, union, unions, (\\))
import qualified Data.Set as Set (map)
import Data.Void (Void)
import Lens.Micro (ix, (^?))
import Text.Megaparsec (MonadParsec (eof), Parsec, parse, some, (<|>))
import Text.Megaparsec.Char (char, eol)
import Prelude hiding (filter, floor)

day16 :: IO ()
day16 = do
  input <- readFile "src/Data/Day16.txt"
  putStr "Day 16 part 1: "
  print . part1 $ input
  putStr "Day 16 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = evalState getEnergizedTiles . initState floor $ Beam {_xPos = 0, _yPos = 0, _dir = East}
  where
    floor = case parse inputParser "" input of
      (Right result) -> result
      _ -> error "Can't parse input!"

part2 :: String -> Int
part2 input = maximum . Set.map (evalState getEnergizedTiles . initState floor) . startBeams $ floor
  where
    floor = case parse inputParser "" input of
      (Right result) -> result
      _ -> error "Can't parse input!"

data Direction = North | West | South | East deriving (Show, Eq, Ord)

data Beam = Beam {_xPos :: Int, _yPos :: Int, _dir :: Direction} deriving (Show, Eq, Ord)

data Tile = NWMirror | SEMirror | VSplitter | HSplitter | Empty deriving (Show, Eq, Ord)

type EnergizedTiles = Set Beam

type Beams = Set Beam

type Floor = [[Tile]]

type Parser = Parsec Void String

tileParser :: Parser Tile
tileParser =
  NWMirror
    <$ char '/'
      <|> SEMirror
    <$ char '\\'
      <|> VSplitter
    <$ char '|'
      <|> HSplitter
    <$ char '-'
      <|> Empty
    <$ char '.'

tileLineParser :: Parser [Tile]
tileLineParser = some tileParser <* eol

inputParser :: Parser Floor
inputParser = some tileLineParser <* eol <* eof

step :: Floor -> Beam -> Beams
step floor beam = case getTileAtPos floor beam of
  (Just tile) -> Set.map adjustPosition . adjustDirection beam $ tile
  Nothing -> empty

adjustDirection :: Beam -> Tile -> Beams
adjustDirection beam Empty = singleton beam
adjustDirection beam NWMirror = case beam._dir of
  North -> singleton beam {_dir = East}
  West -> singleton beam {_dir = South}
  South -> singleton beam {_dir = West}
  East -> singleton beam {_dir = North}
adjustDirection beam SEMirror = case beam._dir of
  North -> singleton beam {_dir = West}
  West -> singleton beam {_dir = North}
  South -> singleton beam {_dir = East}
  East -> singleton beam {_dir = South}
adjustDirection beam VSplitter = case beam._dir of
  West -> fromList [beam {_dir = North}, beam {_dir = South}]
  East -> fromList [beam {_dir = North}, beam {_dir = South}]
  _ -> singleton beam
adjustDirection beam HSplitter = case beam._dir of
  North -> fromList [beam {_dir = West}, beam {_dir = East}]
  South -> fromList [beam {_dir = West}, beam {_dir = East}]
  _ -> singleton beam

adjustPosition :: Beam -> Beam
adjustPosition beam
  | beam._dir == North = beam {_yPos = beam._yPos - 1}
  | beam._dir == West = beam {_xPos = beam._xPos - 1}
  | beam._dir == South = beam {_yPos = beam._yPos + 1}
  | otherwise = beam {_xPos = beam._xPos + 1}

getTileAtPos :: Floor -> Beam -> Maybe Tile
getTileAtPos floor beam = floor ^? ix beam._yPos . ix beam._xPos

getEnergizedTiles :: State (Floor, EnergizedTiles, Beams) Int
getEnergizedTiles = do
  (floor, tiles, beams) <- get
  let unenergizedTiles = beams \\ tiles
  if null unenergizedTiles
    then return . size . Set.map (\beam -> (beam._xPos, beam._yPos)) . filter (isOutside floor) $ tiles
    else do
      let nextBeams = unions . Set.map (step floor) $ unenergizedTiles
      put (floor, tiles `union` unenergizedTiles, nextBeams)
      getEnergizedTiles

initState :: Floor -> Beam -> (Floor, EnergizedTiles, Beams)
initState floor beam = (floor, empty, singleton beam)

isOutside :: Floor -> Beam -> Bool
isOutside floor = isJust . getTileAtPos floor

startBeams :: Floor -> Beams
startBeams floor = unions [northBeams floor, westBeams floor, eastBeams floor, southBeams floor]

beamLine :: [(Int, Int)] -> Direction -> Beams
beamLine range direction = foldl (\beams (x, y) -> insert Beam {_xPos = x, _yPos = y, _dir = direction} beams) empty range

getBounds :: Floor -> (Int, Int)
getBounds floor = ((length . head $ floor) - 1, length floor - 1)

northBeams :: Floor -> Beams
northBeams floor = beamLine range South
  where
    range = map (,0) [0 .. fst . getBounds $ floor]

westBeams :: Floor -> Beams
westBeams floor = beamLine range East
  where
    range = map (0,) [0 .. snd . getBounds $ floor]

eastBeams :: Floor -> Beams
eastBeams floor = beamLine range West
  where
    (xMax, yMax) = getBounds floor
    range = map (xMax,) [0 .. yMax]

southBeams :: Floor -> Beams
southBeams floor = beamLine range North
  where
    (xMax, yMax) = getBounds floor
    range = map (,yMax) [0 .. xMax]
