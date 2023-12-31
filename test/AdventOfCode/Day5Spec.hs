{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day5Spec (spec) where

import AdventOfCode.Day5 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 5" $ do
    it "part one" $
      (readFile "test/Data/Day5.txt" <&> part1) `shouldReturn` 35
    it "part two" $
      (readFile "test/Data/Day5.txt" <&> part2) `shouldReturn` 46
