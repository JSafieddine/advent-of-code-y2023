{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day21Spec (spec) where

import AdventOfCode.Day21 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 21" $ do
    it "part one" $
      (readFile "test/Data/Day21.txt" <&> part1) `shouldReturn` -1
    it "part two" $
      (readFile "test/Data/Day21.txt" <&> part2) `shouldReturn` -1
