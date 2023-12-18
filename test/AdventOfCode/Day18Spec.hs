{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day18Spec (spec) where

import AdventOfCode.Day18 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 18" $ do
    it "part one" $
      (readFile "test/Data/Day18.txt" <&> part1) `shouldReturn` 62
    it "part two" $
      (readFile "test/Data/Day18.txt" <&> part2) `shouldReturn` 952408144115
