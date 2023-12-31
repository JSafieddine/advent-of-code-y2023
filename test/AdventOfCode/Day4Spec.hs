{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day4Spec (spec) where

import AdventOfCode.Day4 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 4" $ do
    it "part one" $
      (readFile "test/Data/Day4.txt" <&> part1) `shouldReturn` 13
    it "part two" $
      (readFile "test/Data/Day4.txt" <&> part2) `shouldReturn` 30
