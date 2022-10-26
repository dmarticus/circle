{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec
  ( describe,
    hspec,
    it,
    parallel,
    shouldBe,
    shouldSatisfy,
  )
import Test.Hspec.Expectations.Contrib (isRight)
import Unknot.Client
import Unknot.Types

testWireAccountDetails :: WireAccountDetails
testWireAccountDetails =
  WireAccountDetails
    (UUID "e553417d-fe7a-4b7a-8d06-ff4de80a0d65")
    [compileAccountNumber|446043103366|]
    (RoutingNumber "021000021")
    ( BillingDetails
        "Test Recipient"
        (City "Snoqualmie")
        (ISO3166Alpha2 unitedStatesOfAmerica)
        (AddressLine "6501 Railroad Avenue SE")
        (Just (AddressLine "Room 315"))
        (Just (District "WA"))
        (PostalCode "85283")
    )
    ( BankAddress
        (Just "Test Bank")
        (Just (City "Snoqualmie"))
        (Just (ISO3166Alpha2 unitedStatesOfAmerica))
        (Just (AddressLine "6501 Railroad Avenue SE"))
        (Just (AddressLine "Room 315"))
        (Just (District "WA"))
    )

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  config <- sandboxEnvConfig
  hspec $
    parallel $ do
      describe "wire accounts" $ do
        describe "create wire account" $ do
          it "creates a new wire account" $ do
            createNewWireResult <- circleTest config manager $ createWireAccount testWireAccountDetails
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
            createdAccount <- circleTest config manager $ createWireAccount testWireAccountDetails
            createdAccount `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseData} = createdAccount
            for_ circleResponseData $ \WireAccountData {..} -> do
              wireAccount <- circleTest config manager $ getWireAccount wireAccountDataId
              wireAccount `shouldSatisfy` isRight
              let Right CircleResponse {circleResponseCode, circleResponseMessage} = wireAccount
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
          it "gets wire instructions for a wire account" $ do
            createdAccount <- circleTest config manager $ createWireAccount testWireAccountDetails
            createdAccount `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseData} = createdAccount
            for_ circleResponseData $ \WireAccountData {..} -> do
              wireAccountInstructions <- circleTest config manager $ getWireAccountInstructions wireAccountDataId
              wireAccountInstructions `shouldSatisfy` isRight
              let Right CircleResponse {circleResponseCode, circleResponseMessage} = wireAccountInstructions
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
      describe "balances" $ do
        describe "list balances" $ do
          it "should list all balances for the newly-created wire account" $ do
            balances <- circleTest config manager listAllBalances
            balances `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseCode, circleResponseMessage} = balances
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      -- TODO should probably actually seed balances, I'll do that when I wrap that API endpoint
      describe "payouts" $ do
        describe "list payouts" $ do
          it "should list all payouts for a given business account" $ do
            -- TODO this param could to be modified to accept query params
            payouts <- circleTest config manager listAllPayouts
            payouts `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseCode, circleResponseMessage} = payouts
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        -- TODO should probably actually seed balances, I'll do that when I wrap that API endpoint
        describe "get payout" $ do
          it "will attempt to return the payout data for the payout Id provided" $ do
            payout <- circleTest config manager (getPayout "e553417d-fe7a-4b7a-8d06-ff4de80a0d65")
            payout `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseCode, circleResponseMessage} = payout
            circleResponseCode `shouldBe` Nothing
            -- will fail if there's no such payout Id
            circleResponseMessage `shouldBe` (Just (ResponseMessage "Resource not found"))
        describe "create payout" $ do
          it "fails to create a new payout because no such account exists" $ do
            let payoutDetailsWithFakeWireAccount =
                  PayoutDetails
                    (UUID "e81b86e4-c4ba-4337-97ff-08486301b618")
                    ( DestinationBankAccount
                        Wire
                        (UUID "6a3a947e-82d2-4204-bf7c-b17d7f380070")
                        Nothing
                    )
                    ( USDOrEURAmount
                        (Amount 100.00)
                        USD
                    )
            -- this request will always fail if no
            failedPayoutResultsNoAccount <- circleTest config manager $ createPayout payoutDetailsWithFakeWireAccount
            failedPayoutResultsNoAccount `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseCode, circleResponseMessage} = failedPayoutResultsNoAccount
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Just (ResponseMessage "Fiat account not found")
          it "fails to create a new payout because the account has insufficient funds" $ do
            -- we first create the wire account so we have an account to send the payout to
            createdAccount <- circleTest config manager $ createWireAccount testWireAccountDetails
            createdAccount `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseData} = createdAccount
            for_ circleResponseData $ \WireAccountData {..} -> do
              -- then, we create a payout
              let payoutDetailsWithRealAccount =
                    PayoutDetails
                      (UUID "e81b86e4-c4ba-4337-97ff-08486301b618")
                      ( DestinationBankAccount
                          Wire
                          wireAccountDataId
                          Nothing
                      )
                      ( USDOrEURAmount
                          (Amount 100.00)
                          USD
                      )
              failedPayoutResultInsufficientFunds <- circleTest config manager $ createPayout payoutDetailsWithRealAccount
              failedPayoutResultInsufficientFunds `shouldSatisfy` isRight
              let Right CircleResponse {circleResponseCode, circleResponseMessage} = failedPayoutResultInsufficientFunds
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Just (ResponseMessage "Account has insufficient funds")
