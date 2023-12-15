{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day15Spec (spec) where

import AdventOfCode.Day15 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 15" $ do
    it "part one" $
      (readFile "test/Data/Day15.txt" <&> part1) `shouldReturn` 1320
    it "part two" $
      (readFile "test/Data/Day15.txt" <&> part2) `shouldReturn` 145
