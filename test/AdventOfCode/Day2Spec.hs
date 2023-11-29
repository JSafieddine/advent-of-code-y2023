{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day2Spec (spec) where

import AdventOfCode.Day2 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, pendingWith, shouldReturn)

spec :: Spec
spec = do
  describe "test day 2" $ do
    it "part one" $
      pendingWith "Not yet implemented!" <* ((readFile "test/Data/Day2.txt" <&> part1) `shouldReturn` -1)
    it "part two" $
      pendingWith "Not yet implemented!" <* ((readFile "test/Data/Day2.txt" <&> part2) `shouldReturn` -1)
