{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: GoodVibes.Core  
-- Description: Core orchestration logic that brings all the good vibes together! 🎭
--
-- This module coordinates all the scanning operations, bringing together pattern
-- matching, git operations, and file scanning to spread security vibes throughout
-- your codebase!
module GoodVibes.Core
  ( -- * Main Scanning Operations
    performScan
  , runScan
  
    -- * Configuration Management
  , validateAndPrepareConfig
  
    -- * Re-exported Types for Convenience
  , module GoodVibes.Types
  ) where

import GoodVibes.Types
import GoodVibes.Scanner
import GoodVibes.Git
import GoodVibes.Utils

import Control.Monad (when)
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.Exit (exitFailure)
import System.IO (putStrLn, hPutStrLn, stderr)

-- | Perform the main scanning operation based on configuration
-- This is where all the scanning vibes come together! ✨
performScan :: ScanConfig -> IO [Finding]
performScan config = do
  let scanActions = []
        ++ [scanCurrentDirectory (repoPath config) | scanCurrent config]
        ++ [scanRepository (repoPath config) (commitCount config) | scanHistory config]
  
  allFindings <- sequence scanActions
  return $ concat allFindings

-- | Run the complete scan workflow with validation and error handling
-- The main orchestration function that spreads good scanning vibes! 🚀
runScan :: ScanConfig -> IO ()
runScan config = do
  -- Validate and prepare configuration
  preparedConfig <- validateAndPrepareConfig config
  
  putStrLn "🔍 Good Vibes Scanner"
  putStrLn $ "Repository: " ++ repoPath preparedConfig
  when (scanCurrent preparedConfig) $ putStrLn "✓ Will scan current working directory"
  when (scanHistory preparedConfig) $ putStrLn $ "✓ Will scan " ++ show (commitCount preparedConfig) ++ " commits from history"
  
  -- Perform the scan
  findings <- performScan preparedConfig
  printFindings findings
  when (not $ null findings) exitFailure

-- | Validate configuration and prepare it for scanning
-- Ensures everything is ready for good vibes scanning! ⚙️
validateAndPrepareConfig :: ScanConfig -> IO ScanConfig
validateAndPrepareConfig config = do
  -- Validate basic configuration
  case validateConfig config of
    Left err -> do
      hPutStrLn stderr $ "❌ Error: " ++ err
      exitFailure
    Right validConfig -> do
      -- Convert to absolute path for better error messages
      absPath <- makeAbsolute (repoPath validConfig)
      
      -- Check if directory exists
      dirExists <- doesDirectoryExist absPath
      when (not dirExists) $ do
        putStrLn $ "❌ Error: Directory does not exist: " ++ absPath
        exitFailure
      
      -- Check if it's a git repository
      isGit <- isGitRepository absPath
      when (not isGit) $ do
        putStrLn $ "❌ Error: Not a git repository: " ++ absPath
        putStrLn "   Make sure the directory contains a .git folder or is inside a git repository."
        exitFailure
      
      return validConfig { repoPath = absPath }