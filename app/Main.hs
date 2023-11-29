module Main (main) where

import AdventOfCode.Day1 (day1)
import AdventOfCode.Day10 (day10)
import AdventOfCode.Day11 (day11)
import AdventOfCode.Day12 (day12)
import AdventOfCode.Day13 (day13)
import AdventOfCode.Day14 (day14)
import AdventOfCode.Day15 (day15)
import AdventOfCode.Day16 (day16)
import AdventOfCode.Day17 (day17)
import AdventOfCode.Day18 (day18)
import AdventOfCode.Day19 (day19)
import AdventOfCode.Day2 (day2)
import AdventOfCode.Day20 (day20)
import AdventOfCode.Day21 (day21)
import AdventOfCode.Day22 (day22)
import AdventOfCode.Day23 (day23)
import AdventOfCode.Day24 (day24)
import AdventOfCode.Day25 (day25)
import AdventOfCode.Day3 (day3)
import AdventOfCode.Day4 (day4)
import AdventOfCode.Day5 (day5)
import AdventOfCode.Day6 (day6)
import AdventOfCode.Day7 (day7)
import AdventOfCode.Day8 (day8)
import AdventOfCode.Day9 (day9)
import Control.Monad (unless, when)
import Data.Char (isDigit)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ error "Invalid argument count!"
  unless (all isDigit (head args)) $ error "Argument is not a decimal numner!"
  let day = read (head args) :: Int
  when (day < 1 || day > 25) $ error "Invalid day!"
  case day of
    1 -> day1
    2 -> day2
    3 -> day3
    4 -> day4
    5 -> day5
    6 -> day6
    7 -> day7
    8 -> day8
    9 -> day9
    10 -> day10
    11 -> day11
    12 -> day12
    13 -> day13
    14 -> day14
    15 -> day15
    16 -> day16
    17 -> day17
    18 -> day18
    19 -> day19
    20 -> day20
    21 -> day21
    22 -> day22
    23 -> day23
    24 -> day24
    25 -> day25
    _ -> error "Invalid day!"
