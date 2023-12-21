{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day20Spec (spec) where

import AdventOfCode.Day20 (part1)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "test day 20" $ do
    it "part one" $
      (readFile "test/Data/Day20-1.txt" <&> part1) `shouldReturn` 32000000

-- Part2 was solved in the REPL by hand.
-- it "part two" $
--  (readFile "test/Data/Day20-1.txt" <&> part2) `shouldReturn` -1
