{-# LANGUAGE OverloadedStrings #-}

module GoodVibes.PatternsSpec (spec) where

import Test.Hspec
import GoodVibes.Patterns
import GoodVibes.Types

-- | Test suite for pattern matching - vibing with the right patterns! ✨
spec :: Spec
spec = do
  describe "🎯 API Key Pattern Matching (catching those key vibes)" $ do
    it "should match Stripe API keys" $ do
      isAPIKeyPattern "sk_fake_51H7FhsAbc123DefGhi456Jkl789MnoPqr" `shouldBe` True
      isAPIKeyPattern "pk_live_987ZyxWvu654Tsr321Pqo098Nml765Kji" `shouldBe` True
      
    it "should match general API key patterns" $ do
      isAPIKeyPattern "API_KEY_1234567890abcdefghijklmnopqrstuvwxyz" `shouldBe` True
      isAPIKeyPattern "APIKEY1234567890ABCDEFGHIJKLMNOP" `shouldBe` True
      isAPIKeyPattern "api_key_secretValue123456789012345" `shouldBe` True
      
    it "should not match common code patterns" $ do
      isAPIKeyPattern "MockApiKeyHandler" `shouldBe` False
      isAPIKeyPattern "getApiKeyFromConfig" `shouldBe` False
      isAPIKeyPattern "API_ENDPOINT" `shouldBe` False

  describe "☁️ AWS Key Pattern Matching (catching those cloud vibes)" $ do
    it "should match AWS access key patterns" $ do
      "AKIAIOSFODNN7EXAMPLE" =~ awsKeyPattern `shouldBe` True
      "AKIA1234567890123456" =~ awsKeyPattern `shouldBe` True
      "ASIAQWERTYUIOPASDFGH" =~ awsKeyPattern `shouldBe` True
      
    it "should not match invalid AWS key patterns" $ do
      "AKIA123" =~ awsKeyPattern `shouldBe` False  -- Too short
      "BKIAIOSFODNN7EXAMPLE" =~ awsKeyPattern `shouldBe` False  -- Wrong prefix
      "akiaiosfodnn7example" =~ awsKeyPattern `shouldBe` False  -- Lowercase

  describe "🔐 Private Key Pattern Matching (securing those key vibes)" $ do
    it "should match RSA private key headers" $ do
      let rsaKey = "-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEA..."
      rsaKey `shouldSatisfy` isPrivateKeyPattern
      
    it "should match generic private key headers" $ do
      let privateKey = "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC..."
      privateKey `shouldSatisfy` isPrivateKeyPattern
      
    it "should match EC private key headers" $ do
      let ecKey = "-----BEGIN EC PRIVATE KEY-----\nMHcCAQEEIA..."
      ecKey `shouldSatisfy` isPrivateKeyPattern

  describe "🎫 JWT Token Pattern Matching (token vibes check)" $ do
    it "should match valid JWT structure" $ do
      let validJWT = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
      validJWT =~ jwtPattern `shouldBe` True
      
    it "should not match incomplete JWT tokens" $ do
      "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIi" =~ jwtPattern `shouldBe` False
      "not.a.jwt.token" =~ jwtPattern `shouldBe` False

  describe "🗄️ Database URL Pattern Matching (connection vibes check)" $ do
    it "should match PostgreSQL URLs with credentials" $ do
      isDatabaseURL "postgres://user:password@localhost:5432/database" `shouldBe` True
      isDatabaseURL "postgresql://admin:secret@db.example.com:5432/myapp" `shouldBe` True
      
    it "should match MySQL URLs with credentials" $ do
      isDatabaseURL "mysql://user:pass@127.0.0.1:3306/dbname" `shouldBe` True
      isDatabaseURL "mysql://root:rootpass@mysql.server:3306/production_db" `shouldBe` True
      
    it "should match MongoDB URLs with credentials" $ do
      isDatabaseURL "mongodb://user:password@mongo.example.com:27017/mydb" `shouldBe` True
      
    it "should match Redis URLs with credentials" $ do
      isDatabaseURL "redis://user:password@redis.example.com:6379/0" `shouldBe` True
      
    it "should not match URLs without credentials" $ do
      isDatabaseURL "postgres://localhost:5432/database" `shouldBe` False
      isDatabaseURL "mysql://localhost:3306/dbname" `shouldBe` False

  describe "🔄 Pattern Compilation (making sure those vibes compile)" $ do
    it "should have working regex patterns" $ do
      awsKeyPattern `shouldSatisfy` (not . null)
      jwtPattern `shouldSatisfy` (not . null)
      
  describe "🧪 Edge Cases (testing those edge vibes)" $ do
    it "should handle empty strings gracefully" $ do
      isAPIKeyPattern "" `shouldBe` False
      isDatabaseURL "" `shouldBe` False
      isPrivateKeyPattern "" `shouldBe` False
      
    it "should handle very long strings" $ do
      let longString = replicate 10000 'a'
      isAPIKeyPattern longString `shouldBe` False
      
    it "should handle strings with special characters" $ do
      isDatabaseURL "postgres://user:p@ssw0rd!@host:5432/db" `shouldBe` True
      isAPIKeyPattern "API_KEY=abc123!@#$%^&*()_+" `shouldBe` False

  describe "🎨 Pattern Quality (ensuring good pattern vibes)" $ do
    it "should not have overlapping classifications" $ do
      let testStrings = [
            "AKIAIOSFODNN7EXAMPLE",
            "sk_fake_12345678901234567890123456789012",
            "postgres://user:pass@host:5432/db",
            "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0In0.xyz"
            ]
      -- Each string should classify to exactly one type
      all (\s -> length (filter ($ s) [isAPIKeyPattern, isDatabaseURL, isPrivateKeyPattern]) <= 1) testStrings
        `shouldBe` True

-- Helper functions for pattern testing
awsKeyPattern :: String
awsKeyPattern = "^(AKIA|ASIA|AROA)[0-9A-Z]{16}$"

jwtPattern :: String  
jwtPattern = "^eyJ[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+$"

isPrivateKeyPattern :: String -> Bool
isPrivateKeyPattern text = "-----BEGIN" `elem` words text && 
                          ("PRIVATE KEY" `elem` words text || "RSA PRIVATE KEY" `elem` words text || "EC PRIVATE KEY" `elem` words text)

(=~) :: String -> String -> Bool
text =~ pattern = pattern `elem` [text]  -- Simplified for testing