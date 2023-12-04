module AdventOfCode.Day4 (day4, part1, part2) where

import Control.Monad.Combinators (someTill)
import Data.List (intersect)
import Data.Map (Map, adjust, empty, insert, (!))
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, parse, some, (<|>))
import Text.Megaparsec.Char (char, eol, hspace, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Card = Card {_Number :: Int, _WinningNumber :: [Int], _YourNumbers :: [Int]} deriving (Show)

type CardStack = Map Int Int

day4 :: IO ()
day4 = do
  input <- readFile "src/Data/Day4.txt"
  putStr "Day 4 part 1: "
  print . part1 $ input
  putStr "Day 4 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = sum . map calcPoint $ cards
  where
    cards = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

part2 :: String -> Int
part2 input = sum . foldl calcCardWinnings (initCardStack cards) $ cards
  where
    cards = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

cardParser :: Parser Card
cardParser =
  Card
    <$> (string "Card" *> hspace *> decimal <* char ':' <* hspace)
    <*> numberParser
    <*> numberParser

numberParser :: Parser [Int]
numberParser = someTill (hspace *> decimal <* hspace) (string "|" <|> eol)

inputParser :: Parser [Card]
inputParser = some cardParser <* eol <* eof

calcPoint :: Card -> Int
calcPoint card = foldl (\w n -> doubleWinning (isWinning n) w) 0 (_YourNumbers card)
  where
    isWinning x = x `elem` _WinningNumber card

doubleWinning :: Bool -> Int -> Int
doubleWinning True 0 = 1
doubleWinning True n = 2 * n
doubleWinning False n = n

winningNumbers :: Card -> Int
winningNumbers card = length $ _WinningNumber card `intersect` _YourNumbers card

initCardStack :: [Card] -> CardStack
initCardStack = foldl func empty
  where
    func stack card = insert (_Number card) 1 stack

calcCardWinnings :: CardStack -> Card -> CardStack
calcCardWinnings stack card = foldl func stack duplicateCardNumbers
  where
    numOfCopies = stack ! _Number card
    duplicateCardNumbers = [_Number card + 1 .. _Number card + winningNumbers card]
    func stack' number = adjust (+ numOfCopies) number stack'
