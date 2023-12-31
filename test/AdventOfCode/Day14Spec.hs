{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day14Spec (spec) where

import AdventOfCode.Day14 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 14" $ do
    it "part one" $
      (readFile "test/Data/Day14.txt" <&> part1) `shouldReturn` 136
    it "part two" $
      (readFile "test/Data/Day14.txt" <&> part2) `shouldReturn` 64
