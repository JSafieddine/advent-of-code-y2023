{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day12Spec (spec) where

import AdventOfCode.Day12 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 12" $ do
    it "part one" $
      (readFile "test/Data/Day12.txt" <&> part1) `shouldReturn` 21
    it "part two" $
      (readFile "test/Data/Day12.txt" <&> part2) `shouldReturn` 525152
