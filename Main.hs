{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isAlphaNum)
import System.Process (readProcessWithExitCode)
import System.Environment (getArgs)
import System.Exit (exitFailure, ExitCode(ExitSuccess))
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.FilePath ((</>))
import Text.Regex.TDFA ((=~))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Data types for our findings
data SecretType = APIKey | AWSKey | PrivateKey | DatabaseURL | JWTToken | GenericSecret
  deriving (Show, Eq)

data Finding = Finding
  { secretType :: SecretType
  , content :: String
  , file :: String
  , commit :: String
  , line :: Int
  } deriving (Show)

-- Pattern matching for different secret types
classifySecret :: String -> Maybe SecretType
classifySecret text
  | "-----BEGIN" `isInfixOf` text && ("PRIVATE KEY" `isInfixOf` text || "RSA PRIVATE KEY" `isInfixOf` text) = Just PrivateKey
  | text =~ ("AKIA[0-9A-Z]{16}" :: String) = Just AWSKey
  | text =~ ("eyJ[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+" :: String) = Just JWTToken
  | isDatabaseURL text = Just DatabaseURL
  | isAPIKeyPattern text = Just APIKey
  | isHighEntropySecret text = Just GenericSecret
  | otherwise = Nothing

-- Pattern matching helper functions
isDatabaseURL :: String -> Bool
isDatabaseURL text = any (`isPrefixOf` text) 
  [ "postgres://", "mysql://", "mongodb://", "redis://", "sqlite://" ]
  && (":" `isInfixOf` text) && ("@" `isInfixOf` text)

isAPIKeyPattern :: String -> Bool
isAPIKeyPattern text = 
  any (\prefix -> prefix `isPrefixOf` text) apiKeyPrefixes &&
  length (filter isAlphaNum text) >= 20
  where
    apiKeyPrefixes = ["sk_", "pk_", "API_KEY", "APIKEY", "api_key"]

isHighEntropySecret :: String -> Bool
isHighEntropySecret text = 
  length text >= 32 && 
  length text <= 128 &&
  all isAlphaNum text &&
  hasGoodEntropy text
  where
    hasGoodEntropy s = 
      let chars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
          charCount = length $ filter (`elem` chars) s
          uniqueChars = length $ filter (`elem` s) chars
      in fromIntegral uniqueChars / fromIntegral charCount > 0.6

-- Extract potential secrets from a line using pattern matching
extractSecrets :: String -> [String]
extractSecrets line = case words line of
  [] -> []
  ws -> concatMap extractFromWord ws
  where
    extractFromWord word
      | "=" `isInfixOf` word = extractAfterEquals word
      | ":" `isInfixOf` word = extractAfterColon word
      | length word > 20 = [word]
      | otherwise = []
    
    extractAfterEquals w = case break (== '=') w of
      (_, '=':value) -> [value]
      _ -> []
    
    extractAfterColon w = case break (== ':') w of
      (_, ':':value) -> [value]
      _ -> []

-- Scan a file content for secrets
scanFileContent :: String -> String -> String -> Int -> [Finding]
scanFileContent commit fileName content startLine = 
  concatMap (scanLine commit fileName) $ zip [startLine..] (lines content)

scanLine :: String -> String -> (Int, String) -> [Finding]
scanLine commit fileName (lineNum, line) = 
  [ Finding sType secret fileName commit lineNum
  | secret <- extractSecrets line
  , Just sType <- [classifySecret secret]
  , not (isCommentedOut line)
  ]

-- Check if line is commented out (basic check)
isCommentedOut :: String -> Bool
isCommentedOut line = 
  let trimmed = dropWhile (== ' ') line
  in any (`isPrefixOf` trimmed) ["//", "#", "--", "/*", "*", "<!--"]

-- Git operations with repository path support
getCommitList :: String -> IO [String]
getCommitList repoPath = do
  (exitCode, output, _) <- readProcessWithExitCode "git" ["-C", repoPath, "log", "--oneline", "--all", "-n", "100"] ""
  case exitCode of
    ExitSuccess -> return $ map (takeWhile (/= ' ')) $ lines output
    _ -> return []

getCommitFiles :: String -> String -> IO [String]
getCommitFiles repoPath commit = do
  (exitCode, output, _) <- readProcessWithExitCode "git" ["-C", repoPath, "show", "--name-only", "--format=", commit] ""
  case exitCode of
    ExitSuccess -> return $ filter (not . null) $ lines output
    _ -> return []

getFileContent :: String -> String -> String -> IO String
getFileContent repoPath commit file = do
  (exitCode, output, _) <- readProcessWithExitCode "git" ["-C", repoPath, "show", commit ++ ":" ++ file] ""
  case exitCode of
    ExitSuccess -> return output
    _ -> return ""

-- Check if a directory is a git repository
isGitRepository :: String -> IO Bool
isGitRepository path = do
  (exitCode, _, _) <- readProcessWithExitCode "git" ["-C", path, "rev-parse", "--git-dir"] ""
  return $ exitCode == ExitSuccess

-- Main scanning logic
scanRepository :: String -> IO [Finding]
scanRepository repoPath = do
  commits <- getCommitList repoPath
  putStrLn $ "Scanning " ++ show (length commits) ++ " commits..."
  findings <- mapM (scanCommit repoPath) (take 50 commits) -- Limit for performance
  return $ concat findings

scanCommit :: String -> String -> IO [Finding]
scanCommit repoPath commit = do
  files <- getCommitFiles repoPath commit
  putStrLn $ "Scanning commit " ++ commit ++ " (" ++ show (length files) ++ " files)"
  findings <- mapM (scanFile repoPath commit) $ filter isTextFile files
  return $ concat findings
  where
    isTextFile file = not $ any (`isInfixOf` file) 
      [".jpg", ".png", ".gif", ".pdf", ".zip", ".tar", ".gz", ".exe", ".dll"]

scanFile :: String -> String -> String -> IO [Finding]
scanFile repoPath commit fileName = do
  content <- getFileContent repoPath commit fileName
  return $ scanFileContent commit fileName content 1

-- Pretty printing results
printFindings :: [Finding] -> IO ()
printFindings [] = putStrLn "✅ No secrets found!"
printFindings findings = do
  putStrLn $ "\n🚨 Found " ++ show (length findings) ++ " potential secrets:\n"
  mapM_ printFinding findings

printFinding :: Finding -> IO ()
printFinding (Finding sType secret file commit lineNum) = do
  putStrLn $ "🔍 " ++ show sType ++ " in " ++ file
  putStrLn $ "   Commit: " ++ commit
  putStrLn $ "   Line: " ++ show lineNum
  putStrLn $ "   Content: " ++ take 50 secret ++ if length secret > 50 then "..." else ""
  putStrLn ""

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "🔍 Git Secret Scanner"
      putStrLn "Scanning current directory..."
      runScan "."
    [path] -> do
      putStrLn "🔍 Git Secret Scanner"
      putStrLn $ "Scanning repository at: " ++ path
      runScan path
    ["--help"] -> printHelp
    _ -> do
      putStrLn "Usage: git-secret-scanner [path] [--help]"
      exitFailure

runScan :: String -> IO ()
runScan inputPath = do
  -- Convert to absolute path for better error messages
  absPath <- makeAbsolute inputPath
  
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
  
  -- Run the scan
  findings <- scanRepository absPath
  printFindings findings
  when (not $ null findings) exitFailure

printHelp :: IO ()
printHelp = do
  putStrLn "Git Secret Scanner - Find accidentally committed secrets"
  putStrLn ""
  putStrLn "Usage: git-secret-scanner [path] [--help]"
  putStrLn ""
  putStrLn "Arguments:"
  putStrLn "  path      Path to git repository (default: current directory)"
  putStrLn "            Can be relative or absolute path"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  git-secret-scanner                    # Scan current directory"
  putStrLn "  git-secret-scanner /path/to/repo      # Scan specific repository"
  putStrLn "  git-secret-scanner ../other-project   # Scan relative path"
  putStrLn ""
  putStrLn "This tool scans your git repository for:"
  putStrLn "  • API Keys (sk_, pk_, API_KEY patterns)"
  putStrLn "  • AWS Access Keys (AKIA...)"
  putStrLn "  • Private Keys (-----BEGIN...)"
  putStrLn "  • Database URLs with credentials"
  putStrLn "  • JWT Tokens"
  putStrLn "  • High entropy strings (potential secrets)"
