{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: GoodVibes.Scanner
-- Description: File scanning operations with good vibes! 📄
--
-- This module handles the actual scanning of files for secrets, whether they're
-- in the current working directory or historical git commits. Spreading those
-- security vibes one file at a time!
module GoodVibes.Scanner
  ( -- * File Scanning Operations
    scanFileContent
  , scanCurrentDirectory
  , scanRepository
  
    -- * Individual File Operations
  , scanCurrentFile
  , scanCommit
  , scanFile
  , scanLine
  
    -- * File Classification  
  , isLikelyTextFile
  , isHighConfidenceSecret
  
    -- * File Discovery
  , getAllFiles
  ) where

import GoodVibes.Types
import GoodVibes.Patterns
import GoodVibes.Git

import Control.Exception (handle, SomeException)
import Control.Monad (when, forM)
import Data.Char (toLower)
import Data.List (isPrefixOf, isInfixOf)
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import System.FilePath ((</>))
import System.IO (putStrLn, IO)
import qualified Prelude as P (readFile)

-- | Scan file content for secrets
-- The core scanning logic that spreads security vibes! 🔍
scanFileContent :: String -> String -> String -> Int -> [Finding]
scanFileContent commit fileName content startLine = 
  let findings = concatMap (scanLine commit fileName) $ zip [startLine..] (lines content)
  in if isTestFile fileName
     then filter isHighConfidenceSecret findings  -- Be more strict with test files
     else findings

-- | Filter to only high-confidence secrets (for test files)
-- Maintains good vibes by being extra careful with test file findings! ✅
isHighConfidenceSecret :: Finding -> Bool
isHighConfidenceSecret (Finding sType _ _ _ _) = sType `elem` [AWSKey, PrivateKey, JWTToken, DatabaseURL]

-- | Scan a single line for potential secrets
-- Where the line-by-line magic happens! ⚡
scanLine :: String -> String -> (Int, String) -> [Finding]
scanLine commit fileName (lineNum, line) = 
  [ Finding sType secret fileName commit lineNum
  | secret <- extractSecrets line
  , Just sType <- [classifySecret secret]
  , not (isCommentedOut line)
  , not (isLikelyFalsePositive fileName line secret)
  ]

-- | Scan current working directory for secrets
-- Brings good vibes to your current codebase! 📁
scanCurrentDirectory :: String -> IO [Finding]
scanCurrentDirectory repoPath = do
  putStrLn "📁 Scanning current working directory..."
  files <- getAllFiles repoPath
  let textFiles = filter isLikelyTextFile files
  putStrLn $ "Found " ++ show (length textFiles) ++ " text files in working directory"
  when (length textFiles < length files) $
    putStrLn $ "  Skipping " ++ show (length files - length textFiles) ++ " binary/non-text files"
  findings <- mapM (scanCurrentFile repoPath) textFiles
  return $ concat findings

-- | Get all files in directory recursively (excluding .git and other ignored paths)
-- Discovers all those files that need good security vibes! 🗂️
getAllFiles :: String -> IO [String]
getAllFiles dir = do
  contents <- listDirectory dir
  let validContents = filter (not . isIgnoredPath) contents
  files <- forM validContents $ \item -> do
    let fullPath = dir </> item
    isFile <- doesFileExist fullPath
    isDir <- doesDirectoryExist fullPath
    if isFile
      then return [item]
      else if isDir
        then do
          subFiles <- getAllFiles fullPath
          return $ map (item </>) subFiles
        else return []
  return $ concat files
  where
    isIgnoredPath path = any (`isPrefixOf` path) 
      [".git", ".svn", ".hg", "node_modules", "target", "dist", "build", ".gradle", ".mvn"]

-- | Scan a file from current working directory  
-- Brings current file scanning vibes! 📄
scanCurrentFile :: String -> String -> IO [Finding]
scanCurrentFile repoPath fileName = do
  let fullPath = repoPath </> fileName
  result <- handle (\(_ :: SomeException) -> return []) $ do
    content <- P.readFile fullPath
    if isBinaryContent content
      then return []
      else return $ scanFileContent "CURRENT" fileName content 1
  return result

-- | Scan repository history for secrets
-- Dives deep into git history for those historical vibes! 📚
scanRepository :: String -> Int -> IO [Finding]
scanRepository repoPath commitCount = do
  commits <- getCommitList repoPath commitCount
  putStrLn $ "📚 Scanning " ++ show (length commits) ++ " commits from history..."
  findings <- mapM (scanCommit repoPath) commits
  return $ concat findings

-- | Check if file is likely to be text based on extension and path
-- Keeps those text analysis vibes focused! 📝
isLikelyTextFile :: String -> Bool
isLikelyTextFile file = 
  let lowerFile = map (toLower) file
  in not (any (`isInfixOf` lowerFile) binaryExtensions) && 
     not (any (`isPrefixOf` lowerFile) binaryPrefixes)
  where
    binaryExtensions = [
      ".jpg", ".jpeg", ".png", ".gif", ".bmp", ".ico", ".svg",
      ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".ppt", ".pptx",
      ".zip", ".tar", ".gz", ".bz2", ".7z", ".rar",
      ".exe", ".dll", ".so", ".dylib", ".bin", ".dat",
      ".mp3", ".mp4", ".avi", ".mov", ".wmv", ".flv",
      ".ttf", ".woff", ".woff2", ".eot",
      ".class", ".jar", ".war", ".ear",
      ".dmg", ".iso", ".img"
      ]
    
    binaryPrefixes = [
      "node_modules/", ".git/", "vendor/", "target/",
      "build/", "dist/", ".gradle/", ".mvn/"
      ]

-- | Scan a specific commit for secrets
-- Brings those historical commit vibes to life! 📜
scanCommit :: String -> String -> IO [Finding]
scanCommit repoPath commit = do
  files <- getCommitFiles repoPath commit
  putStrLn $ "📚 Scanning commit " ++ take 8 commit ++ " (" ++ show (length files) ++ " files)"
  let textFiles = filter isLikelyTextFile files
  when (length textFiles < length files) $
    putStrLn $ "  Skipping " ++ show (length files - length textFiles) ++ " binary/non-text files"
  findings <- mapM (scanFile repoPath commit) textFiles
  return $ concat findings

-- | Scan a specific file in a specific commit
-- File-level scanning vibes for git history! 🔍
scanFile :: String -> String -> String -> IO [Finding]
scanFile repoPath commit fileName = do
  content <- getFileContent repoPath commit fileName
  return $ scanFileContent commit fileName content 1