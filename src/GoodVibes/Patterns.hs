{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: GoodVibes.Patterns  
-- Description: Pattern matching for secret detection with good vibes! 🎯
--
-- This module contains all the pattern matching logic for identifying different
-- types of secrets. Each pattern is crafted with care to catch real secrets
-- while maintaining good vibes by avoiding false positives!
module GoodVibes.Patterns
  ( -- * Main Classification Function
    classifySecret
    
    -- * Specific Pattern Matchers
  , isAPIKeyPattern
  , isDatabaseURL
  , isHighEntropySecret
  
    -- * Helper Functions for Pattern Analysis
  , isCamelCase
  , isLikelyCodeIdentifier
  , calculateEntropy
  
    -- * Content Analysis
  , extractSecrets
  , isCommentedOut
  , isLikelyFalsePositive
  ) where

import GoodVibes.Types
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isAlphaNum, isDigit, toLower, isAsciiLower, isAsciiUpper)
import Text.Regex.TDFA ((=~))

-- | Classify a string as a potential secret type
-- This is where the magic happens - good vibes meet pattern recognition! ✨
classifySecret :: String -> Maybe SecretType
classifySecret text
  | "-----BEGIN" `isInfixOf` text && ("PRIVATE KEY" `isInfixOf` text || "RSA PRIVATE KEY" `isInfixOf` text) = Just PrivateKey
  | text =~ ("AKIA[0-9A-Z]{16}" :: String) = Just AWSKey
  | text =~ ("eyJ[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+" :: String) = Just JWTToken
  | isDatabaseURL text = Just DatabaseURL
  | isAPIKeyPattern text = Just APIKey
  | isHighEntropySecret text = Just GenericSecret
  | otherwise = Nothing

-- | Check if a string matches API key patterns
-- Designed to catch real API keys while keeping code identifiers chill! 🔑
isAPIKeyPattern :: String -> Bool
isAPIKeyPattern text = 
  any (`isPrefixOf` text) apiKeyPrefixes &&
  length (filter isAlphaNum text) >= 20 &&
  not (isCamelCase text) &&
  not (isLikelyCodeIdentifier text)
  where
    apiKeyPrefixes = ["sk_", "pk_", "API_KEY", "APIKEY", "api_key"]

-- | Detect database URLs that contain credentials
-- Connection strings with secrets deserve special attention! 🗄️
isDatabaseURL :: String -> Bool
isDatabaseURL text = any (`isPrefixOf` text) 
  [ "postgres://", "mysql://", "mongodb://", "redis://", "sqlite://" ]
  && (":" `isInfixOf` text) && ("@" `isInfixOf` text)

-- | Detect high-entropy strings that might be secrets
-- Uses mathematical entropy to spot those random-looking vibes! 🎲
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
          lowerCount = length $ filter isAsciiLower s
          upperCount = length $ filter isAsciiUpper s
          hasDigits = digitCount > 0
          hasLowers = lowerCount > 0
          hasUppers = upperCount > 0
          entropy = calculateEntropy s
      in hasDigits && hasLowers && hasUppers && entropy > 3.5

-- | Check if string follows CamelCase pattern (legitimate code identifier)
-- Keeps those coding vibes flowing without false alarms! 🐫
isCamelCase :: String -> Bool
isCamelCase text = 
  length text > 4 &&
  startsWithUpper text &&
  hasMultipleCapitalLetters text &&
  not (hasConsecutiveUppercase text) &&
  hasReasonableWordBoundaries text
  where
    startsWithUpper (c:_) = isAsciiUpper c
    startsWithUpper [] = False
    
    hasMultipleCapitalLetters s = length (filter isAsciiUpper s) >= 2
    
    hasConsecutiveUppercase s = any isConsecutiveUpper (zip s (tail s))
    isConsecutiveUpper (c1, c2) = isAsciiUpper c1 && isAsciiUpper c2
    
    hasReasonableWordBoundaries s = 
      let upperPositions = [i | (i, c) <- zip [0..] s, isAsciiUpper c]
          avgDistance = if length upperPositions > 1 
                       then fromIntegral (last upperPositions - head upperPositions) / fromIntegral (length upperPositions - 1)
                       else 0
      in avgDistance > 2 && avgDistance < 15

-- | Check if string looks like a code identifier 
-- Recognizes common coding patterns to maintain good vibes! 👨‍💻
isLikelyCodeIdentifier :: String -> Bool
isLikelyCodeIdentifier text =
  isSnakeCase text ||
  hasCommonCodePatterns text ||
  isAllUpperWithUnderscores text
  where
    isSnakeCase s = "_" `isInfixOf` s && all (\c -> isAlphaNum c || c == '_') s
    isAllUpperWithUnderscores s = all (\c -> isAsciiUpper c || c == '_' || isDigit c) s
    hasCommonCodePatterns s = any (`isInfixOf` map toLower s) commonCodeWords
    commonCodeWords = ["test", "mock", "handler", "service", "client", "server", "config", "util", "helper", "manager", "controller", "processor", "validator", "builder", "factory", "pipeline", "workflow", "template", "model", "entity", "dto", "request", "response", "exception", "error", "logger", "cache", "database", "repository", "interface", "abstract", "implementation", "adapter"]

-- | Calculate entropy of a string (higher = more random)
-- Mathematics meets security vibes! 📊
calculateEntropy :: String -> Double
calculateEntropy s = 
  let counts = map (\c -> length $ filter (== c) s) ['a'..'z'] ++ 
               map (\c -> length $ filter (== c) s) ['A'..'Z'] ++ 
               map (\c -> length $ filter (== c) s) ['0'..'9']
      total = length s
      probs = [fromIntegral count / fromIntegral total | count <- counts, count > 0]
  in negate $ sum [p * logBase 2 p | p <- probs]

-- | Extract potential secrets from a line using pattern matching
-- Finds those hidden secrets in various formats! 🔍
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

-- | Check if line is commented out (basic check)
-- Respects those comment vibes! 💬
isCommentedOut :: String -> Bool
isCommentedOut line = 
  let trimmed = dropWhile (== ' ') line
  in any (`isPrefixOf` trimmed) ["//", "#", "--", "/*", "*", "<!--"]

-- | Check if this is likely a false positive based on context
-- Keeps the noise down and the good vibes up! ✨
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

-- | Check if this is a test file (be more lenient with test files)
-- Test files get special treatment for good testing vibes! 🧪
isTestFile :: String -> Bool
isTestFile file = 
  let lowerFile = map toLower file
  in any (`isInfixOf` lowerFile) ["test", "spec", "mock", "__test__", ".test.", ".spec."]