{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day8Spec (spec) where

import AdventOfCode.Day8 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 8" $ do
    it "part one" $
      (readFile "test/Data/Day8-1.txt" <&> part1) `shouldReturn` 6
    it "part two" $
      (readFile "test/Data/Day8-2.txt" <&> part2) `shouldReturn` 6
