{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day17Spec (spec) where

import AdventOfCode.Day17 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 17" $ do
    it "part one" $
      (readFile "test/Data/Day17.txt" <&> part1) `shouldReturn` 102
    it "part two" $
      (readFile "test/Data/Day17.txt" <&> part2) `shouldReturn` 94
