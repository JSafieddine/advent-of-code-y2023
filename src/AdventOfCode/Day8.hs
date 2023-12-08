{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AdventOfCode.Day8 (day8, part1, part2) where

import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)), cycle, fromList)
import Data.Map.Strict (Map, empty, insert, keys, (!))
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, parse, some, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, eol, space, string)
import Prelude hiding (cycle)

type Parser = Parsec Void String

data Instruction = L | R deriving (Show)

data Node = Node {_name :: String, _left :: String, _right :: String} deriving (Show, Eq)

type Network = Map String Node

data Path = Path {_instructions :: NonEmpty Instruction, _network :: Network, _currentNode :: String, _steps :: Int} deriving (Show)

data GhostPath = GhostPath {_ghostInstructions :: [Instruction], _ghostNetwork :: Network, _currentNodes :: [String], _ghostSteps :: Int} deriving (Show)

day8 :: IO ()
day8 = do
  input <- readFile "src/Data/Day8.txt"
  putStr "Day 8 part 1: "
  print . part1 $ input
  putStr "Day 8 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = step $ initPath instructions network
  where
    (instructions, network) = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid Input!"

part2 :: String -> Int
part2 input = foldl1 lcm . map (ghostStep . initGhostPath instructions network) . getInitNodes $ network
  where
    (instructions, network) = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid Input!"

leftParser :: Parser Instruction
leftParser = char 'L' $> L

rightParser :: Parser Instruction
rightParser = char 'R' $> R

instructionParser :: Parser Instruction
instructionParser = try leftParser <|> rightParser

nodeParser :: Parser Node
nodeParser =
  Node
    <$> (some alphaNumChar <* string " = (")
    <*> (some alphaNumChar <* string ", ")
    <*> (some alphaNumChar <* char ')' <* eol)

networkParser :: Parser Network
networkParser = do
  nodes <- some nodeParser
  eol
  eof
  return . foldl addNode empty $ nodes
  where
    addNode nw node = insert (_name node) node nw

inputParser :: Parser ([Instruction], Network)
inputParser = (,) <$> some instructionParser <* space <*> networkParser

initPath :: [Instruction] -> Network -> Path
initPath inst nw = Path {_instructions = cycle . fromList $ inst, _network = nw, _currentNode = "AAA", _steps = 0}

step :: Path -> Int
step p
  | currentNode == "ZZZ" = _steps p
  | otherwise = step p {_instructions = fromList rest, _currentNode = nextNode, _steps = succ . _steps $ p}
  where
    currentNode = p._currentNode
    (inst :| rest) = p._instructions
    nextNode = inst2Dir inst $ p._network ! currentNode

inst2Dir :: Instruction -> (Node -> String)
inst2Dir L = _left
inst2Dir R = _right

initGhostPath :: [Instruction] -> Network -> String -> Path
initGhostPath inst nw startNode = Path {_instructions = cycle . fromList $ inst, _network = nw, _currentNode = startNode, _steps = 0}

ghostGoalReached :: String -> Bool
ghostGoalReached = (== 'Z') . last

ghostStep :: Path -> Int
ghostStep p
  | ghostGoalReached currentNode = p._steps
  | otherwise = ghostStep p {_instructions = fromList rest, _currentNode = nextNode, _steps = succ p._steps}
  where
    currentNode = p._currentNode
    (inst :| rest) = p._instructions
    nextNode = inst2Dir inst $ p._network ! currentNode

getInitNodes :: Network -> [String]
getInitNodes = filter ((== 'A') . last) . keys
