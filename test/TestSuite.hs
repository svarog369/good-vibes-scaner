{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test suite for Good Vibes Scaner - keeping those vibes tested! ✨
module Main where

import Test.Hspec
import Test.QuickCheck
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isAlphaNum, isDigit, toLower)
import Text.Regex.TDFA ((=~))

-- Import the actual implementation from our library
import qualified GoodVibesLib as GV

-- Test suite
main :: IO ()
main = hspec $ do
  describe "🔍 Good Vibes Scaner Test Suite" $ do
    
    describe "🎯 Secret Classification (vibing with the right types)" $ do
      it "should classify AWS keys with good vibes" $ do
        GV.classifySecret "AKIAIOSFODNN7EXAMPLE" `shouldBe` Just GV.AWSKey
        GV.classifySecret "AKIA1234567890123456" `shouldBe` Just GV.AWSKey
        
      it "should classify private keys with secure vibes" $ do
        GV.classifySecret "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBg..." `shouldBe` Just GV.PrivateKey
        GV.classifySecret "-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQ..." `shouldBe` Just GV.PrivateKey
        
      it "should classify JWT tokens with token vibes" $ do
        GV.classifySecret "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c" `shouldBe` Just GV.JWTToken
        
      it "should classify database URLs with connection vibes" $ do
        GV.classifySecret "postgres://user:password@localhost:5432/database" `shouldBe` Just GV.DatabaseURL
        GV.classifySecret "mysql://admin:secret123@db.example.com:3306/mydb" `shouldBe` Just GV.DatabaseURL
        
      it "should classify API keys with good pattern vibes" $ do
        -- Use a pk_ prefix which is less likely to be filtered as code
        GV.classifySecret "pk_1234567890abcdefghijklmnop" `shouldBe` Just GV.APIKey
        
      it "should not classify regular code identifiers (keeping false positives chill)" $ do
        GV.classifySecret "MockCancelRideWithCustomerPipeline" `shouldBe` Nothing
        GV.classifySecret "getUserProfileDataFromDatabase" `shouldBe` Nothing

    describe "🧠 False Positive Filtering (keeping vibes clean)" $ do
      it "should identify CamelCase patterns correctly" $ do
        GV.isCamelCase "MockApiResponseHandler" `shouldBe` True
        GV.isCamelCase "UserProfileManager" `shouldBe` True
        GV.isCamelCase "lowercaseonly" `shouldBe` False
        GV.isCamelCase "UPPERCASEONLY" `shouldBe` False
        
      it "should identify code patterns correctly" $ do
        GV.isLikelyCodeIdentifier "user_profile_service" `shouldBe` True
        GV.isLikelyCodeIdentifier "TEST_DATABASE_URL" `shouldBe` True

    describe "🎯 Database URL Detection (connection vibes check)" $ do
      it "should match PostgreSQL URLs with credentials" $ do
        GV.isDatabaseURL "postgres://user:password@localhost:5432/database" `shouldBe` True
        
      it "should match MySQL URLs with credentials" $ do
        GV.isDatabaseURL "mysql://user:pass@127.0.0.1:3306/dbname" `shouldBe` True
        
      it "should not match URLs without credentials" $ do
        GV.isDatabaseURL "postgres://localhost:5432/database" `shouldBe` False
        GV.isDatabaseURL "mysql://localhost:3306/dbname" `shouldBe` False

    describe "🎲 Property-based Testing (random good vibes)" $ do
      it "entropy should be non-negative" $ property $
        \str -> not (null str) ==> GV.calculateEntropy str >= 0
        
      it "CamelCase detection should be consistent" $ property $
        \str -> length str > 4 ==> 
          let result1 = GV.isCamelCase str
              result2 = GV.isCamelCase str
          in result1 == result2

    describe "✨ Integration Tests (end-to-end good vibes)" $ do
      it "should detect multiple secret types in realistic scenarios" $ do
        -- Test data that should be detected
        let testSecrets = [
              "pk_1234567890abcdefghijklmnop",
              "postgres://user:secretpassword123@localhost:5432/mydb", 
              "AKIA1234567890123456"
              ]
        all (\s -> GV.classifySecret s /= Nothing) testSecrets `shouldBe` True
        
      it "should not detect common code patterns" $ do
        -- Test data that should NOT be detected  
        let codePatterns = [
              "MockApiResponseHandler",
              "getUserFromDatabase"
              ]
        all (\s -> GV.classifySecret s == Nothing) codePatterns `shouldBe` True