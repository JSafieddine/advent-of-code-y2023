{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day10Spec (spec) where

import AdventOfCode.Day10 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 10" $ do
    it "part one" $
      (readFile "test/Data/Day10-1.txt" <&> part1) `shouldReturn` 4
    it "part two" $
      (readFile "test/Data/Day10-2.txt" <&> part2) `shouldReturn` 4
    it "part two 2" $
      (readFile "test/Data/Day10-3.txt" <&> part2) `shouldReturn` 8
    it "part two 3" $
      (readFile "test/Data/Day10-4.txt" <&> part2) `shouldReturn` 10
