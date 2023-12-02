{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day2Spec (spec) where

import AdventOfCode.Day2 (drawParser, part1, part2)
import Data.Functor ((<&>))
import Data.Map (fromList)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "test day 2" $ do
    it "drawParser 1 " $
      parse drawParser "" "1 red, 2 blue, 3 green"
        `shouldParse` fromList
          [ ("red", 1),
            ("green", 3),
            ("blue", 2)
          ]
    it "part one" $
      (readFile "test/Data/Day2.txt" <&> part1) `shouldReturn` 8
    it "part two" $
      (readFile "test/Data/Day2.txt" <&> part2) `shouldReturn` 2286
