{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day16Spec (spec) where

import AdventOfCode.Day16 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 16" $ do
    it "part one" $
      (readFile "test/Data/Day16.txt" <&> part1) `shouldReturn` 46
    it "part two" $
      (readFile "test/Data/Day16.txt" <&> part2) `shouldReturn` 51
