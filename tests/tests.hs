{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

-- import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Unknot.Client
import Unknot.Types
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec
import Test.Hspec.Expectations.Contrib (isRight)

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
        createNewWireResult <- circleTest config manager $ createWireAccount exampleWireAccountDetails
        createNewWireResult `shouldSatisfy` isRight
        let Right CircleResponse {..} = createNewWireResult
        circleResponseCode `shouldBe` Nothing
        circleResponseMessage `shouldBe` Nothing
    describe "get wire accounts" $ do
      it "gets a list of wire accounts" $ do
        getWireAccountsResults <- circleTest config manager getWireAccounts
        getWireAccountsResults `shouldSatisfy` isRight
        let Right CircleResponse {..} = getWireAccountsResults
        circleResponseCode `shouldBe` Nothing
        circleResponseMessage `shouldBe` Nothing
    describe "get wire account" $ do
      it "gets a single wire account" $ do
        createdAccount <- circleTest config manager $ createWireAccount exampleWireAccountDetails
        createdAccount `shouldSatisfy` isRight
        let Right CircleResponse {circleResponseData} = createdAccount
        for_ circleResponseData $ \WireAccountData {..} -> do
          wireAccount <- circleTest config manager $ getWireAccount wireAccountDataId
          wireAccount `shouldSatisfy` isRight
          let Right CircleResponse {circleResponseCode, circleResponseMessage} = wireAccount
          circleResponseCode `shouldBe` Nothing
          circleResponseMessage `shouldBe` Nothing
      it "gets wire instructions for a wire account" $ do
        createdAccount <- circleTest config manager $ createWireAccount exampleWireAccountDetails
        createdAccount `shouldSatisfy` isRight
        let Right CircleResponse {circleResponseData} = createdAccount
        for_ circleResponseData $ \WireAccountData {..} -> do
          wireAccountInstructions <- circleTest config manager $ getWireAccountInstructions wireAccountDataId
          wireAccountInstructions `shouldSatisfy` isRight
          let Right CircleResponse {circleResponseCode, circleResponseMessage} = wireAccountInstructions
          circleResponseCode `shouldBe` Nothing
          circleResponseMessage `shouldBe` Nothing