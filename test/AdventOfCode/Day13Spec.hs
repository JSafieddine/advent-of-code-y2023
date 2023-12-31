{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day13Spec (spec) where

import AdventOfCode.Day13 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 13" $ do
    it "part one" $
      (readFile "test/Data/Day13.txt" <&> part1) `shouldReturn` 405
    it "part two" $
      (readFile "test/Data/Day13.txt" <&> part2) `shouldReturn` 400
