{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day6Spec (spec) where

import AdventOfCode.Day6 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 6" $ do
    it "part one" $
      (readFile "test/Data/Day6.txt" <&> part1) `shouldReturn` 288
    it "part two" $
      (readFile "test/Data/Day6.txt" <&> part2) `shouldReturn` 71503
