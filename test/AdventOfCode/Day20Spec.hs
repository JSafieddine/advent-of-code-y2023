{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day20Spec (spec) where

import AdventOfCode.Day20 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, pendingWith, shouldReturn)

spec :: Spec
spec = do
  describe "test day 20" $ do
    it "part one" $
      pendingWith "Not yet implemented!" <* ((readFile "test/Data/Day20.txt" <&> part1) `shouldReturn` -1)
    it "part two" $
      pendingWith "Not yet implemented!" <* ((readFile "test/Data/Day20.txt" <&> part2) `shouldReturn` -1)
