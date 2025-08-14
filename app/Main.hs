{-# LANGUAGE OverloadedStrings #-}

-- |
-- Main entry point for Good Vibes Scaner! ✨
-- 
-- This is where the good vibes journey begins - spreading security awareness
-- through your git repositories with positive energy and emojis!
module Main where

import GoodVibes.Core
import GoodVibes.Utils
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- | Main function - where the good vibes start flowing! 🚀
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left "help" -> printHelp
    Left err -> do
      putStrLn $ "❌ Error: " ++ err
      putStrLn "Use --help for usage information"
      exitFailure
    Right config -> runScan config