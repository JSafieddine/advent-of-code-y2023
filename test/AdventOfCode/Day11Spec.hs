{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day11Spec (spec) where

import AdventOfCode.Day11 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 11" $ do
    it "part one" $
      (readFile "test/Data/Day11.txt" <&> part1) `shouldReturn` 374
    it "part two" $
      (readFile "test/Data/Day11.txt" <&> part2) `shouldReturn` 82000210
