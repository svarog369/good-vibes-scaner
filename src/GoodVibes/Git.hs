{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: GoodVibes.Git
-- Description: Git repository operations with good vibes! 🚀
--
-- This module handles all interactions with git repositories, spreading good
-- vibes through version control operations while maintaining security focus.
module GoodVibes.Git
  ( -- * Repository Operations
    isGitRepository
  , getCommitList
  , getCommitFiles
  , getFileContent
  
    -- * Content Analysis
  , isBinaryContent
  ) where

import Control.Exception (handle, SomeException)
import Data.Char (ord)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))

-- | Check if a directory is a git repository
-- Verifies those git vibes are present! 📁
isGitRepository :: String -> IO Bool
isGitRepository path = do
  (exitCode, _, _) <- readProcessWithExitCode "git" ["-C", path, "rev-parse", "--git-dir"] ""
  return $ exitCode == ExitSuccess

-- | Get list of commit hashes from repository
-- Retrieves commit history for good scanning vibes! 📚
getCommitList :: String -> Int -> IO [String]
getCommitList repoPath commitCount = do
  (exitCode, output, _) <- readProcessWithExitCode "git" 
    ["-C", repoPath, "log", "--oneline", "--all", "-n", show commitCount] ""
  case exitCode of
    ExitSuccess -> return $ map (takeWhile (/= ' ')) $ lines output
    _ -> return []

-- | Get list of files changed in a specific commit
-- Discovers which files were touched by those commit vibes! 📝
getCommitFiles :: String -> String -> IO [String]
getCommitFiles repoPath commit = do
  (exitCode, output, _) <- readProcessWithExitCode "git" 
    ["-C", repoPath, "show", "--name-only", "--format=", commit] ""
  case exitCode of
    ExitSuccess -> return $ filter (not . null) $ lines output
    _ -> return []

-- | Get content of a file at a specific commit
-- Retrieves historical file content for those scanning vibes! 🔍
getFileContent :: String -> String -> String -> IO String
getFileContent repoPath commit file = do
  handle (\(_ :: SomeException) -> return "") $ do
    (exitCode, output, _) <- readProcessWithExitCode "git" 
      ["-C", repoPath, "show", commit ++ ":" ++ file] ""
    case exitCode of
      ExitSuccess -> 
        if isBinaryContent output
          then return ""  -- Skip binary content for good vibes
          else return output
      _ -> return ""

-- | Check if content appears to be binary
-- Keeps those binary vibes separate from text analysis! 🔢
isBinaryContent :: String -> Bool
isBinaryContent content = 
  let sampleSize = min 1000 (length content)
      sample = take sampleSize content
      nullBytes = length $ filter (== '\0') sample
      highBytes = length $ filter (\c -> ord c > 127) sample
      totalBytes = length sample
  in totalBytes > 0 && 
     (nullBytes > 0 || fromIntegral highBytes / fromIntegral totalBytes > 0.3)