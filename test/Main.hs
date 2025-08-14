{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified GoodVibes.CoreSpec as Core
import qualified GoodVibes.PatternsSpec as Patterns

-- | Main test entry point - spreading good vibes through testing! ✨
main :: IO ()
main = hspec $ do
  describe "🔍 Good Vibes Scaner Test Suite" $ do
    describe "Core functionality (keeping those vibes strong)" Core.spec
    describe "Pattern matching (vibing with the right patterns)" Patterns.spec