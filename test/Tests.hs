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
import Test.Hspec.Expectations.Contrib (isRight)
import Unknot.Client
import Unknot.Types

testWireAccountDetails :: WireAccountBodyParams
testWireAccountDetails =
  WireAccountBodyParams
    (UUID "e553417d-fe7a-4b7a-8d06-ff4de80a0d65")
    [compileAccountNumber|446043103366|]
    [compileRoutingNumber|021000021|]
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

testSubscriptionBody :: SubscriptionBodyParams
testSubscriptionBody =
  -- TODO this fucking URL doesn't work
  SubscriptionBodyParams "https://example.org/handler/for/notifications"

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  config <- sandboxEnvConfig
  hspec $
    parallel $ do
      describe "overview endpoints" $ do
        describe "management" $ do
          it "gets configuration info" $ do
            configurationInfo <- circleTest config manager getConfigurationInfo
            let Right CircleResponse {..} = configurationInfo
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "encryption" $ do
          it "gets public key info" $ do
            keyInfo <- circleTest config manager getPublicKey
            let Right CircleResponse {..} = keyInfo
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "channels" $ do
          it "lists all channels" $ do
            channels <- circleTest config manager listAllChannels
            let Right CircleResponse {..} = channels
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "stablecoins" $ do
          it "lists all stablecoins" $ do
            stablecoins <- circleTest config manager listAllStablecoins
            let Right CircleResponse {..} = stablecoins
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "subscriptions" $ do
          it "creates a new subscription" $ do
            subscription <- circleTest config manager $ createSubscription testSubscriptionBody
            let Right CircleResponse {..} = subscription
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Just (ResponseMessage "Unable to complete request. One or more request parameters are invalid.")
          it "deletes a subscription" $ do
            deletionResponse <- circleTest config manager $ deleteSubscription (UUID "e553417d-fe7a-4b7a-8d06-ff4de80a0d65")
            let Right CircleResponse {..} = deletionResponse
            circleResponseCode `shouldBe` Nothing
            -- TODO we don't have a resource so it'll fail
            circleResponseMessage `shouldBe` Just (ResponseMessage "Resource not found")
          it "lists all subscription" $ do
            subscriptions <- circleTest config manager listAllNotificationSubscriptions
            let Right CircleResponse {..} = subscriptions
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "wire endpoints" $ do
        describe "create wire account" $ do
          it "creates a new wire account" $ do
            newWireAccount <- circleTest config manager $ createWireAccount testWireAccountDetails
            newWireAccount `shouldSatisfy` isRight
            let Right CircleResponse {..} = newWireAccount
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get wire accounts" $ do
          it "gets a list of wire accounts" $ do
            wireAccounts <- circleTest config manager getWireAccounts
            wireAccounts `shouldSatisfy` isRight
            let Right CircleResponse {..} = wireAccounts
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get wire account" $ do
          it "gets a single wire account" $ do
            wireAccount1 <- circleTest config manager $ createWireAccount testWireAccountDetails
            wireAccount1 `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseData} = wireAccount1
            for_ circleResponseData $ \WireAccountData {..} -> do
              wireAccount <- circleTest config manager $ getWireAccount wireAccountDataId
              wireAccount `shouldSatisfy` isRight
              let Right CircleResponse {circleResponseCode, circleResponseMessage} = wireAccount
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
          it "gets wire instructions for a wire account" $ do
            wireAccount2 <- circleTest config manager $ createWireAccount testWireAccountDetails
            wireAccount2 `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseData} = wireAccount2
            for_ circleResponseData $ \WireAccountData {..} -> do
              wireAccountInstructions <- circleTest config manager $ getWireAccountInstructions wireAccountDataId
              wireAccountInstructions `shouldSatisfy` isRight
              -- liftIO $ print wireAccountInstructions
              let Right CircleResponse {circleResponseCode, circleResponseMessage} = wireAccountInstructions
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
      describe "balance endpoints" $ do
        -- TODO need to actually seed balances, I'll do that when I wrap that API endpoint
        describe "list balances" $ do
          it "should list all balances for the newly-created wire account" $ do
            balances <- circleTest config manager listAllBalances
            balances `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseCode, circleResponseMessage} = balances
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "transfer endpoints" $ do
        describe "list transfers" $ do
          it "should list all transfers for a given business account" $ do
            transfers <- circleTest config manager listAllTransfers
            transfers `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseCode, circleResponseMessage} = transfers
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get transfer" $ do
          it "will attempt to return transfer data for a single transfer" $ do
            transfer <- circleTest config manager (getTransfer "e553417d-fe7a-4b7a-8d06-ff4de80a0d65")
            transfer `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseCode, circleResponseMessage} = transfer
            circleResponseCode `shouldBe` Nothing
            -- will fail if there's no such payout Id
            circleResponseMessage `shouldBe` Just (ResponseMessage "Resource not found")
        describe "create transfer" $ do
          it "will attempt to create a new transfer" $ do
            let transferBody =
                  TransferBodyParams
                    (UUID "c14bf1a2-74fe-4cd5-8e74-c8c67903d849")
                    (TransferBodyDestination
                      VerifiedBlockchain
                      (UUID "2206775d-e4f7-4681-9494-34dc650fbfd8"))
                    (CurrencyAmount
                      (Amount 100.00)
                      USD
                    )
            -- this request will always fail if there's no money in the account
            idkWhatThisWillBe <- circleTest config manager $ createTransfer transferBody
            idkWhatThisWillBe `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseCode, circleResponseMessage} = idkWhatThisWillBe
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Just (ResponseMessage "Address not found")
        
      describe "payout endpoints" $ do
        -- TODO need to actually seed balances, I'll do that when I wrap that API endpoint
        describe "list payouts" $ do
          -- fit " should list a subset of payouts for a given business account given the query params" $ do
          --   payoutsBeforeFoo <- circleTest config manager $ listAllPayouts -&- PaginationQueryParams (PageSize "foo")
          --   payoutsBeforeFoo `shouldSatisfy` isRight
          --   let Right CircleResponse {circleResponseCode, circleResponseMessage} = payoutsBeforeFoo
          --   circleResponseCode `shouldBe` Nothing
          --   circleResponseMessage `shouldBe` Nothing
          it "should list all payouts for a given business account" $ do
            payouts <- circleTest config manager listAllPayouts
            payouts `shouldSatisfy` isRight
            let Right CircleResponse {circleResponseCode, circleResponseMessage} = payouts
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
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
            let payoutWithFakeWireAccount =
                  PayoutBodyParams
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
            -- this request will always fail if there's no money in the account
            failedPayoutResultsNoAccount <- circleTest config manager $ createPayout payoutWithFakeWireAccount
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
              let payoutWithRealWireAccount =
                    PayoutBodyParams
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
              failedPayoutResultInsufficientFunds <- circleTest config manager $ createPayout payoutWithRealWireAccount
              failedPayoutResultInsufficientFunds `shouldSatisfy` isRight
              let Right CircleResponse {circleResponseCode, circleResponseMessage} = failedPayoutResultInsufficientFunds
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Just (ResponseMessage "Account has insufficient funds")
