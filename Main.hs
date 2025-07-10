{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (when, forM)
import Control.Exception (handle, SomeException)
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isAlphaNum, isDigit, ord, chr, toLower)
import System.Process (readProcessWithExitCode)
import System.Environment (getArgs)
import System.Exit (exitFailure, ExitCode(ExitSuccess))
import System.Directory (doesDirectoryExist, makeAbsolute, listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (readFile)
import qualified Prelude as P (readFile)

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

-- Configuration for scanning
data ScanConfig = ScanConfig
  { repoPath :: String
  , commitCount :: Int
  , scanCurrent :: Bool
  , scanHistory :: Bool
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

isAPIKeyPattern :: String -> Bool
isAPIKeyPattern text = 
  any (\prefix -> prefix `isPrefixOf` text) apiKeyPrefixes &&
  length (filter isAlphaNum text) >= 20 &&
  not (isCamelCase text) &&  -- Don't flag CamelCase as API keys
  not (isLikelyCodeIdentifier text)
  where
    apiKeyPrefixes = ["sk_", "pk_", "API_KEY", "APIKEY", "api_key"]

-- Pattern matching helper functions
isDatabaseURL :: String -> Bool
isDatabaseURL text = any (`isPrefixOf` text) 
  [ "postgres://", "mysql://", "mongodb://", "redis://", "sqlite://" ]
  && (":" `isInfixOf` text) && ("@" `isInfixOf` text)

isHighEntropySecret :: String -> Bool
isHighEntropySecret text = 
  length text >= 32 && 
  length text <= 128 &&
  all isAlphaNum text &&
  not (isCamelCase text) &&
  not (isLikelyCodeIdentifier text) &&
  hasGoodEntropy text
  where
    hasGoodEntropy s = 
      let digitCount = length $ filter isDigit s
          lowerCount = length $ filter (\c -> c >= 'a' && c <= 'z') s
          upperCount = length $ filter (\c -> c >= 'A' && c <= 'Z') s
          totalChars = length s
          -- Real secrets tend to have more balanced character distribution
          hasDigits = digitCount > 0
          hasLowers = lowerCount > 0
          hasUppers = upperCount > 0
          -- Check for randomness - secrets usually have less predictable patterns
          entropy = calculateEntropy s
      in hasDigits && hasLowers && hasUppers && entropy > 3.5

-- Check if string follows CamelCase pattern (legitimate code identifier)
isCamelCase :: String -> Bool
isCamelCase text = 
  length text > 4 &&
  startsWithUpper text &&
  hasMultipleCapitalLetters text &&
  not (hasConsecutiveUppercase text) &&
  hasReasonableWordBoundaries text
  where
    startsWithUpper (c:_) = c >= 'A' && c <= 'Z'
    startsWithUpper [] = False
    
    hasMultipleCapitalLetters s = length (filter (\c -> c >= 'A' && c <= 'Z') s) >= 2
    
    hasConsecutiveUppercase s = any isConsecutiveUpper (zip s (tail s))
    isConsecutiveUpper (c1, c2) = (c1 >= 'A' && c1 <= 'Z') && (c2 >= 'A' && c2 <= 'Z')
    
    hasReasonableWordBoundaries s = 
      let upperPositions = [i | (i, c) <- zip [0..] s, c >= 'A' && c <= 'Z']
          avgDistance = if length upperPositions > 1 
                       then fromIntegral (last upperPositions - head upperPositions) / fromIntegral (length upperPositions - 1)
                       else 0
      in avgDistance > 2 && avgDistance < 15

-- Check if string looks like a code identifier (variable, function, class name)
isLikelyCodeIdentifier :: String -> Bool
isLikelyCodeIdentifier text =
  isSnakeCase text ||
  hasCommonCodePatterns text ||
  isAllUpperWithUnderscores text
  where
    isSnakeCase s = "_" `isInfixOf` s && all (\c -> isAlphaNum c || c == '_') s
    isAllUpperWithUnderscores s = all (\c -> (c >= 'A' && c <= 'Z') || c == '_' || isDigit c) s
    hasCommonCodePatterns s = any (`isInfixOf` map toLower s) commonCodeWords
    commonCodeWords = ["test", "mock", "handler", "service", "client", "server", "config", "util", "helper", "manager", "controller", "processor", "validator", "builder", "factory", "pipeline", "workflow", "template", "model", "entity", "dto", "request", "response", "exception", "error", "logger", "cache", "database", "repository", "interface", "abstract", "implementation", "adapter"]

-- Calculate entropy of a string (higher = more random)
calculateEntropy :: String -> Double
calculateEntropy s = 
  let counts = map (\c -> length $ filter (== c) s) ['a'..'z'] ++ 
               map (\c -> length $ filter (== c) s) ['A'..'Z'] ++ 
               map (\c -> length $ filter (== c) s) ['0'..'9']
      total = length s
      probs = [fromIntegral count / fromIntegral total | count <- counts, count > 0]
  in negate $ sum [p * logBase 2 p | p <- probs]

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
  let findings = concatMap (scanLine commit fileName) $ zip [startLine..] (lines content)
  in if isTestFile fileName
     then filter isHighConfidenceSecret findings  -- Be more strict with test files
     else findings

-- Filter to only high-confidence secrets (for test files)
isHighConfidenceSecret :: Finding -> Bool
isHighConfidenceSecret (Finding sType _ _ _ _) = sType `elem` [AWSKey, PrivateKey, JWTToken, DatabaseURL]

scanLine :: String -> String -> (Int, String) -> [Finding]
scanLine commit fileName (lineNum, line) = 
  [ Finding sType secret fileName commit lineNum
  | secret <- extractSecrets line
  , Just sType <- [classifySecret secret]
  , not (isCommentedOut line)
  , not (isLikelyFalsePositive fileName line secret)
  ]

-- Check if this is likely a false positive based on context
isLikelyFalsePositive :: String -> String -> String -> Bool
isLikelyFalsePositive fileName line secret =
  isInTestFile fileName ||
  isInLogStatement line ||
  isInDocumentation line ||
  isClassOrFunctionName line secret ||
  isImportOrRequire line
  where
    lowerLine = map toLower line
    
    isInTestFile f = isTestFile f
    
    isInLogStatement l = any (`isInfixOf` lowerLine) 
      ["console.log", "print(", "println", "log.info", "log.debug", "logger.", "logging."]
    
    isInDocumentation l = any (`isInfixOf` lowerLine) 
      ["@param", "@return", "//", "/*", "/**", "<!--", "readme", "documentation"]
    
    isClassOrFunctionName l s = any (`isInfixOf` lowerLine) 
      ["class ", "function ", "def ", "const ", "let ", "var ", "interface ", "type "] &&
      isCamelCase s
    
    isImportOrRequire l = any (`isInfixOf` lowerLine) 
      ["import ", "require(", "from ", "include ", "using "]

-- Check if line is commented out (basic check)
isCommentedOut :: String -> Bool
isCommentedOut line = 
  let trimmed = dropWhile (== ' ') line
  in any (`isPrefixOf` trimmed) ["//", "#", "--", "/*", "*", "<!--"]

-- Git operations with repository path support
getCommitList :: String -> Int -> IO [String]
getCommitList repoPath commitCount = do
  (exitCode, output, _) <- readProcessWithExitCode "git" ["-C", repoPath, "log", "--oneline", "--all", "-n", show commitCount] ""
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
  handle (\(_ :: SomeException) -> return "") $ do
    (exitCode, output, _) <- readProcessWithExitCode "git" ["-C", repoPath, "show", commit ++ ":" ++ file] ""
    case exitCode of
      ExitSuccess -> 
        if isBinaryContent output
          then return ""  -- Skip binary content
          else return output
      _ -> return ""

-- Check if content appears to be binary
isBinaryContent :: String -> Bool
isBinaryContent content = 
  let sampleSize = min 1000 (length content)
      sample = take sampleSize content
      nullBytes = length $ filter (== '\0') sample
      highBytes = length $ filter (\c -> ord c > 127) sample
      totalBytes = length sample
  in totalBytes > 0 && 
     (nullBytes > 0 || fromIntegral highBytes / fromIntegral totalBytes > 0.3)

-- Check if a directory is a git repository
isGitRepository :: String -> IO Bool
isGitRepository path = do
  (exitCode, _, _) <- readProcessWithExitCode "git" ["-C", path, "rev-parse", "--git-dir"] ""
  return $ exitCode == ExitSuccess

-- Scan current working directory
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

-- Get all files in directory recursively (excluding .git)
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
    isIgnoredPath path = any (`isPrefixOf` path) [".git", ".svn", ".hg", "node_modules", "target", "dist", "build", ".gradle", ".mvn"]

-- Scan a file from current working directory
scanCurrentFile :: String -> String -> IO [Finding]
scanCurrentFile repoPath fileName = do
  let fullPath = repoPath </> fileName
  result <- handle (\(_ :: SomeException) -> return []) $ do
    content <- P.readFile fullPath
    if isBinaryContent content
      then return []
      else return $ scanFileContent "CURRENT" fileName content 1
  return result

-- Main scanning logic
scanRepository :: String -> Int -> IO [Finding]
scanRepository repoPath commitCount = do
  commits <- getCommitList repoPath commitCount
  putStrLn $ "📚 Scanning " ++ show (length commits) ++ " commits from history..."
  findings <- mapM (scanCommit repoPath) commits
  return $ concat findings

-- Check if file is likely to be text based on extension and path
isLikelyTextFile :: String -> Bool
isLikelyTextFile file = 
  let lowerFile = map toLower file
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

-- Check if this is a test file (be more lenient with test files)
isTestFile :: String -> Bool
isTestFile file = 
  let lowerFile = map toLower file
  in any (`isInfixOf` lowerFile) ["test", "spec", "mock", "__test__", ".test.", ".spec."]

scanCommit :: String -> String -> IO [Finding]
scanCommit repoPath commit = do
  files <- getCommitFiles repoPath commit
  putStrLn $ "📚 Scanning commit " ++ take 8 commit ++ " (" ++ show (length files) ++ " files)"
  let textFiles = filter isLikelyTextFile files
  when (length textFiles < length files) $
    putStrLn $ "  Skipping " ++ show (length files - length textFiles) ++ " binary/non-text files"
  findings <- mapM (scanFile repoPath commit) textFiles
  return $ concat findings

scanFile :: String -> String -> String -> IO [Finding]
scanFile repoPath commit fileName = do
  content <- getFileContent repoPath commit fileName
  return $ scanFileContent commit fileName content 1

-- Combined scanning based on configuration
performScan :: ScanConfig -> IO [Finding]
performScan config = do
  let scanActions = []
        ++ [scanCurrentDirectory (repoPath config) | scanCurrent config]
        ++ [scanRepository (repoPath config) (commitCount config) | scanHistory config]
  
  allFindings <- sequence scanActions
  return $ concat allFindings

-- Argument parsing
parseArgs :: [String] -> Either String ScanConfig
parseArgs [] = Right $ ScanConfig "." 50 True True  -- Default: current dir, 50 commits, scan both
parseArgs ["--help"] = Left "help"
parseArgs args = parseArgsHelper args (ScanConfig "." 50 True True)

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

-- Pretty printing results
printFindings :: [Finding] -> IO ()
printFindings [] = putStrLn "✅ No secrets found!"
printFindings findings = do
  putStrLn $ "\n🚨 Found " ++ show (length findings) ++ " potential secrets:\n"
  mapM_ printFinding findings

printFinding :: Finding -> IO ()
printFinding (Finding sType secret file commit lineNum) = do
  putStrLn $ "🔍 " ++ show sType ++ " in " ++ file
  if commit == "CURRENT"
    then putStrLn "   Location: Current working directory"
    else putStrLn $ "   Commit: " ++ take 8 commit
  putStrLn $ "   Line: " ++ show lineNum
  putStrLn $ "   Content: " ++ take 60 secret ++ if length secret > 60 then "..." else ""
  putStrLn ""

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left "help" -> printHelp
    Left err -> do
      putStrLn $ "❌ Error: " ++ err
      putStrLn "Use --help for usage information"
      exitFailure
    Right config -> do
      putStrLn "🔍 Git Secret Scanner"
      putStrLn $ "Repository: " ++ repoPath config
      when (scanCurrent config) $ putStrLn "✓ Will scan current working directory"
      when (scanHistory config) $ putStrLn $ "✓ Will scan " ++ show (commitCount config) ++ " commits from history"
      when (not (scanCurrent config) && not (scanHistory config)) $ do
        putStrLn "❌ Error: Nothing to scan! Use --help for options."
        exitFailure
      runScan config

runScan :: ScanConfig -> IO ()
runScan config = do
  -- Convert to absolute path for better error messages
  absPath <- makeAbsolute (repoPath config)
  
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
  findings <- performScan config { repoPath = absPath }
  printFindings findings
  when (not $ null findings) exitFailure

printHelp :: IO ()
printHelp = do
  putStrLn "Git Secret Scanner - Find accidentally committed secrets"
  putStrLn ""
  putStrLn "Usage: git-secret-scanner [options] [path] [commits] [--help]"
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
  putStrLn "  git-secret-scanner                         # Scan current dir + 50 commits"
  putStrLn "  git-secret-scanner --current-only          # Only scan current working files"
  putStrLn "  git-secret-scanner --history-only 100      # Only scan 100 commits from history"
  putStrLn "  git-secret-scanner /path/to/repo 200       # Scan specific repo, current + 200 commits"
  putStrLn "  git-secret-scanner --no-current 25         # Only scan 25 commits (no current files)"
  putStrLn "  git-secret-scanner ../project --no-history # Only scan current files in ../project"
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
