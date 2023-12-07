{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day7Spec (spec) where

import AdventOfCode.Day7 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 7" $ do
    it "part one" $
      (readFile "test/Data/Day7.txt" <&> part1) `shouldReturn` 6440
    it "part two" $
      (readFile "test/Data/Day7.txt" <&> part2) `shouldReturn` 5905
