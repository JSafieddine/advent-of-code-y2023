{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day25Spec (spec) where

import AdventOfCode.Day25 (part1, part2)
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, pendingWith, shouldReturn)

spec :: Spec
spec = do
  describe "test day 25" $ do
    it "part one" $
      pendingWith "Not yet implemented!" <* ((readFile "test/Data/Day25.txt" <&> part1) `shouldReturn` -1)
    it "part two" $
      pendingWith "Not yet implemented!" <* ((readFile "test/Data/Day25.txt" <&> part2) `shouldReturn` -1)
