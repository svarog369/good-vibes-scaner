{-# LANGUAGE OverloadedStrings #-}

module GoodVibes.CoreSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import GoodVibes.Core
import GoodVibes.Types

-- | Test suite for core functionality - keeping those vibes tested! ✨
spec :: Spec
spec = do
  describe "🔍 Secret Classification (vibing with the right types)" $ do
    it "should classify AWS keys with good vibes" $ do
      classifySecret "AKIAIOSFODNN7EXAMPLE" `shouldBe` Just AWSKey
      classifySecret "AKIA1234567890123456" `shouldBe` Just AWSKey
      
    it "should classify private keys with secure vibes" $ do
      classifySecret "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBg..." `shouldBe` Just PrivateKey
      classifySecret "-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQ..." `shouldBe` Just PrivateKey
      
    it "should classify JWT tokens with token vibes" $ do
      classifySecret "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c" `shouldBe` Just JWTToken
      
    it "should classify database URLs with connection vibes" $ do
      classifySecret "postgres://user:password@localhost:5432/database" `shouldBe` Just DatabaseURL
      classifySecret "mysql://admin:secret123@db.example.com:3306/mydb" `shouldBe` Just DatabaseURL
      
    it "should classify API keys with good pattern vibes" $ do
      classifySecret "sk_fake_51H7FhsAbc123DefGhi456Jkl789MnoPqr" `shouldBe` Just APIKey
      classifySecret "pk_live_987ZyxWvu654Tsr321Pqo098Nml" `shouldBe` Just APIKey
      
    it "should not classify regular code identifiers (keeping false positives chill)" $ do
      classifySecret "MockCancelRideWithCustomerPipeline" `shouldBe` Nothing
      classifySecret "getUserProfileDataFromDatabase" `shouldBe` Nothing
      classifySecret "API_ENDPOINT_URL" `shouldBe` Nothing

  describe "🧠 False Positive Filtering (keeping vibes clean)" $ do
    it "should identify CamelCase patterns correctly" $ do
      isCamelCase "MockApiResponseHandler" `shouldBe` True
      isCamelCase "UserProfileManager" `shouldBe` True
      isCamelCase "lowercaseonly" `shouldBe` False
      isCamelCase "UPPERCASEONLY" `shouldBe` False
      
    it "should identify code patterns correctly" $ do
      isLikelyCodeIdentifier "user_profile_service" `shouldBe` True
      isLikelyCodeIdentifier "TEST_DATABASE_URL" `shouldBe` True
      isLikelyCodeIdentifier "randomSecretKey123456789" `shouldBe` False

  describe "🎯 Entropy Analysis (measuring those random vibes)" $ do
    it "should calculate entropy correctly for various strings" $ do
      calculateEntropy "aaaaaaaaaa" `shouldSatisfy` (< 1.0)  -- Low entropy
      calculateEntropy "abcdefghijklmnopqrstuvwxyz" `shouldSatisfy` (> 4.0)  -- High entropy
      calculateEntropy "Abc123XyZ789" `shouldSatisfy` (\x -> x > 3.0 && x < 4.0)  -- Medium entropy
      
    it "should detect high entropy secrets with good vibes" $ do
      isHighEntropySecret "sk9f8h2n4k6j8m1q3r7t9w2e5y8i0o3p6s1d4g7h0k3l6z9x" `shouldBe` True
      isHighEntropySecret "password123" `shouldBe` False
      isHighEntropySecret "MockTestServiceHandler" `shouldBe` False

  describe "📁 File Filtering (vibing with the right files)" $ do
    it "should identify text files correctly" $ do
      isLikelyTextFile "app.js" `shouldBe` True
      isLikelyTextFile "config.json" `shouldBe` True
      isLikelyTextFile "README.md" `shouldBe` True
      isLikelyTextFile "image.png" `shouldBe` False
      isLikelyTextFile "binary.exe" `shouldBe` False
      
    it "should identify test files correctly" $ do
      isTestFile "app.test.js" `shouldBe` True
      isTestFile "UserSpec.hs" `shouldBe` True
      isTestFile "mock_data.py" `shouldBe` True
      isTestFile "production.js" `shouldBe` False

  describe "🔍 Secret Extraction (pulling out those hidden vibes)" $ do
    it "should extract secrets from assignment patterns" $ do
      extractSecrets "API_KEY=sk_fake_12345678901234567890123456789012" 
        `shouldContain` ["sk_fake_12345678901234567890123456789012"]
      extractSecrets "password: secretpassword123456789" 
        `shouldContain` ["secretpassword123456789"]
        
    it "should extract secrets from configuration lines" $ do
      extractSecrets "database_url = postgres://user:pass@host:5432/db"
        `shouldContain` ["postgres://user:pass@host:5432/db"]

-- Property-based tests for extra good vibes
  describe "🎲 Property-based Testing (random good vibes)" $ do
    it "entropy should be non-negative" $ property $
      \str -> not (null str) ==> calculateEntropy str >= 0
      
    it "CamelCase detection should be consistent" $ property $
      \str -> length str > 4 ==> 
        let result1 = isCamelCase str
            result2 = isCamelCase str
        in result1 == result2