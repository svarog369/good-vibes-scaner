{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: GoodVibes.Utils
-- Description: Utility functions that spread good vibes throughout the codebase! 🛠️
--
-- This module contains helper functions that support the main scanning operations
-- while maintaining those good vibes and clean code practices.
module GoodVibes.Utils
  ( -- * Output Functions
    printFindings
  , printFinding
  , printHelp
  
    -- * Argument Parsing
  , parseArgs
  , parseArgsHelper
  
    -- * Validation Functions
  , validateConfig
  ) where

import GoodVibes.Types
import Data.Char (isDigit)
import Text.Read (readMaybe)

-- | Pretty print all findings with good vibes
-- Makes security findings look friendly and approachable! ✨
printFindings :: [Finding] -> IO ()
printFindings [] = putStrLn "✅ No secrets found!"
printFindings findings = do
  putStrLn $ "\n🚨 Found " ++ show (length findings) ++ " potential secrets:\n"
  mapM_ printFinding findings

-- | Print a single finding with good vibes formatting
-- Individual finding display with positive energy! 🔍
printFinding :: Finding -> IO ()
printFinding (Finding sType secret fileName commitHash lineNum) = do
  putStrLn $ "🔍 " ++ show sType ++ " in " ++ fileName
  if commitHash == "CURRENT"
    then putStrLn "   Location: Current working directory"
    else putStrLn $ "   Commit: " ++ take 8 commitHash
  putStrLn $ "   Line: " ++ show lineNum
  putStrLn $ "   Content: " ++ take 60 secret ++ if length secret > 60 then "..." else ""
  putStrLn ""

-- | Print help message with good vibes
-- Help documentation that spreads positive energy! 📚
printHelp :: IO ()
printHelp = do
  putStrLn "Git Secret Scanner - Find accidentally committed secrets"
  putStrLn ""
  putStrLn "Usage: good-vibes-scaner [options] [path] [commits] [--help]"
  putStrLn ""
  putStrLn "Arguments:"
  putStrLn "  path      Path to git repository (default: current directory)"
  putStrLn "  commits   Number of commits to scan (default: 50)"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --current-only    Only scan current working directory (no history)"
  putStrLn "  --history-only    Only scan git history (no current files)"
  putStrLn "  --no-current      Don't scan current working directory"
  putStrLn "  --no-history      Don't scan git history"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  good-vibes-scaner                         # Scan current dir + 50 commits"
  putStrLn "  good-vibes-scaner --current-only          # Only scan current working files"
  putStrLn "  good-vibes-scaner --history-only 100      # Only scan 100 commits from history"
  putStrLn "  good-vibes-scaner /path/to/repo 200       # Scan specific repo, current + 200 commits"
  putStrLn "  good-vibes-scaner --no-current 25         # Only scan 25 commits (no current files)"
  putStrLn "  good-vibes-scaner ../project --no-history # Only scan current files in ../project"
  putStrLn ""
  putStrLn "This tool scans for:"
  putStrLn "  • API Keys (sk_, pk_, API_KEY patterns)"
  putStrLn "  • AWS Access Keys (AKIA...)"
  putStrLn "  • Private Keys (-----BEGIN...)"
  putStrLn "  • Database URLs with credentials"
  putStrLn "  • JWT Tokens"
  putStrLn "  • High entropy strings (potential secrets)"
  putStrLn ""
  putStrLn "Scan targets:"
  putStrLn "  📁 Current working directory - files as they are right now"
  putStrLn "  📚 Git history - files from commit snapshots"
  putStrLn ""
  putStrLn "Smart filtering (reduces false positives):"
  putStrLn "  • Ignores CamelCase identifiers (e.g., MockCancelRideWithCustomerPipeline)"
  putStrLn "  • Skips common code patterns (snake_case, function names, etc.)"
  putStrLn "  • Less strict on test files"
  putStrLn "  • Filters out log statements and documentation"
  putStrLn "  • Uses entropy analysis to detect real secrets"
  putStrLn ""
  putStrLn "Performance notes:"
  putStrLn "  • Current directory scanning is fast"
  putStrLn "  • History scanning can be slow with many commits"
  putStrLn "  • Use --current-only for quick scans before committing"

-- | Parse command line arguments into configuration
-- Argument parsing that maintains good vibes! 🎛️
parseArgs :: [String] -> Either String ScanConfig
parseArgs [] = Right defaultScanConfig
parseArgs ["--help"] = Left "help"
parseArgs args = parseArgsHelper args defaultScanConfig

-- | Helper function for recursive argument parsing
-- Recursively processes arguments with good vibes! 🔄
parseArgsHelper :: [String] -> ScanConfig -> Either String ScanConfig
parseArgsHelper [] config = Right config
parseArgsHelper (arg:rest) config
  | arg == "--current-only" = parseArgsHelper rest $ config { scanCurrent = True, scanHistory = False }
  | arg == "--history-only" = parseArgsHelper rest $ config { scanCurrent = False, scanHistory = True }
  | arg == "--no-current" = parseArgsHelper rest $ config { scanCurrent = False }
  | arg == "--no-history" = parseArgsHelper rest $ config { scanHistory = False }
  | all isDigit arg = case readMaybe arg of
      Just n | n > 0 -> parseArgsHelper rest $ config { commitCount = n }
      _ -> Left "Invalid commit count"
  | otherwise = parseArgsHelper rest $ config { repoPath = arg }

-- | Validate scan configuration
-- Ensures configuration maintains good vibes! ✅
validateConfig :: ScanConfig -> Either String ScanConfig
validateConfig config
  | not (scanCurrent config) && not (scanHistory config) = 
      Left "Nothing to scan! Enable either current directory or history scanning."
  | commitCount config <= 0 = 
      Left "Commit count must be positive for good vibes!"
  | null (repoPath config) = 
      Left "Repository path cannot be empty!"
  | otherwise = Right config