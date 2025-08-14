{-# LANGUAGE OverloadedStrings #-}

-- | 
-- Module: GoodVibes.Types
-- Description: Core data types and configurations for spreading good vibes! ✨
-- 
-- This module contains all the essential types that keep our good vibes flowing
-- through the secret scanning process.
module GoodVibes.Types
  ( -- * Secret Classification Types
    SecretType(..)
  , Finding(..)
  
    -- * Configuration Types  
  , ScanConfig(..)
  
    -- * Default Values
  , defaultScanConfig
  ) where

-- | Types of secrets we can detect (each with their own special vibes!) 
data SecretType 
  = APIKey        -- ^ API keys with good authentication vibes
  | AWSKey        -- ^ AWS access keys for cloud vibes
  | PrivateKey    -- ^ Private keys for cryptographic vibes  
  | DatabaseURL   -- ^ Database connection strings with credential vibes
  | JWTToken      -- ^ JWT tokens for authorization vibes
  | GenericSecret -- ^ High-entropy strings with mysterious vibes
  deriving (Show, Eq, Ord)

-- | A finding represents a potential secret discovered during scanning
-- Each finding carries the vibes of what was found and where!
data Finding = Finding
  { secretType :: SecretType  -- ^ What kind of secret vibes we detected
  , content    :: String      -- ^ The actual secret content (handle with care!)
  , file       :: String      -- ^ Which file was spreading these secret vibes
  , commit     :: String      -- ^ Git commit hash (or "CURRENT" for working directory)
  , line       :: Int         -- ^ Line number where the vibes were found
  } deriving (Show, Eq)

-- | Configuration for scanning operations
-- Customize how you want to spread those security vibes!
data ScanConfig = ScanConfig
  { repoPath     :: String  -- ^ Path to the git repository to scan
  , commitCount  :: Int     -- ^ Number of commits to analyze (more commits = deeper vibes)
  , scanCurrent  :: Bool    -- ^ Whether to scan current working directory files
  , scanHistory  :: Bool    -- ^ Whether to scan git commit history
  } deriving (Show, Eq)

-- | Default configuration with balanced good vibes
-- Perfect for getting started with secret scanning!
defaultScanConfig :: ScanConfig
defaultScanConfig = ScanConfig
  { repoPath    = "."    -- Current directory vibes
  , commitCount = 50     -- 50 commits of historical vibes
  , scanCurrent = True   -- Current file vibes enabled
  , scanHistory = True   -- Historical vibes enabled
  }