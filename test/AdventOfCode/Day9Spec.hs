{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day9Spec (spec) where

import AdventOfCode.Day9 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 9" $ do
    it "part one" $
      (readFile "test/Data/Day9.txt" <&> part1) `shouldReturn` 114
    it "part two" $
      (readFile "test/Data/Day9.txt" <&> part2) `shouldReturn` 2
