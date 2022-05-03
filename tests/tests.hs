{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

-- import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (for_)
import           Unknot.Client
import           Unknot.Types
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib (isRight)

exampleWireAccountDetails :: WireAccountDetails
exampleWireAccountDetails =
  WireAccountDetails
    (ExternalId "e553417d-fe7a-4b7a-8d06-ff4de80a0d65")
    (AccountNumber "123456789")
    (RoutingNumber "021000021")
    (BillingDetails
      "Test Recipient"
      (City "Snoqualmie")
      (Country "US")
      (AddressLine "6501 Railroad Avenue SE")
      (Just (AddressLine "Room 315"))
      (Just (District "WA"))
      (PostalCode "85283"))
    (BankAddress 
      (Just "Test Bank")
      (Just (City "Snoqualmie"))
      (Just (Country "US"))
      (Just (AddressLine "6501 Railroad Avenue SE"))
      (Just (AddressLine "Room 315"))
      (Just (District "WA")))

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  config <- sandboxEnvConfig
  hspec $ parallel $ do
    describe "create wire account" $ do
      it "creates a new wire account" $ do
        result <- circleTest config manager $ createWireAccount exampleWireAccountDetails
        result `shouldSatisfy` isRight
        let Right CircleResponse {..} = result
        circleResponseCode `shouldBe` Nothing
        circleResponseMessage `shouldBe` Nothing
    describe "get wire accounts" $ do
      it "gets a list of wire accounts" $ do
        result <- circleTest config manager $ getWireAccounts
        result `shouldSatisfy` isRight
        let Right CircleResponse {..} = result
        circleResponseCode `shouldBe` Nothing
        circleResponseMessage `shouldBe` Nothing
    describe "get wire account" $ do
      it "gets a single wire account" $ do
        createdAccount <- circleTest config manager $ createWireAccount exampleWireAccountDetails
        createdAccount `shouldSatisfy` isRight
        let Right CircleResponse {circleResponseData} = createdAccount
        for_ circleResponseData $ \WireAccountData {..} -> do
          result <- circleTest config manager $ getWireAccount wireAccountDataId
          result `shouldSatisfy` isRight
          let Right CircleResponse {circleResponseCode, circleResponseMessage} = result
          circleResponseCode `shouldBe` Nothing
          circleResponseMessage `shouldBe` Nothing

