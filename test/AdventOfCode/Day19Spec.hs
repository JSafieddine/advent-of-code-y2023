{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day19Spec (spec) where

import AdventOfCode.Day19 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 19" $ do
    it "part one" $
      (readFile "test/Data/Day19.txt" <&> part1) `shouldReturn` 19114
    it "part two" $
      (readFile "test/Data/Day19.txt" <&> part2) `shouldReturn` 167409079868000
