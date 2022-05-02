{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
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
        liftIO $ print result
        result `shouldSatisfy` isRight
        let Right CircleResponse {..} = result
        circleResponseCode `shouldBe` Nothing
        circleResponseMessage `shouldBe` Nothing
