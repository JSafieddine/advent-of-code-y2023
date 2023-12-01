{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day1Spec (spec) where

import AdventOfCode.Day1 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 1" $ do
    it "part one" $
      (readFile "test/Data/Day1-1.txt" <&> part1) `shouldReturn` 142
    it "part two" $
      (readFile "test/Data/Day1-2.txt" <&> part2) `shouldReturn` 281
