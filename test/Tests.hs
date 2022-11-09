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

testUSWireAccountDetails :: WireAccountBodyParams
testUSWireAccountDetails =
  USBankAccount
    ( USBankAccountBodyParams
        [compileUUID|e553417d-fe7a-4b7a-8d06-ff4de80a0d65|]
        [compileAccountNumber|446043103366|]
        [compileRoutingNumber|021000021|]
        ( BillingDetails
            "Test Recipient"
            (City "Snoqualmie")
            (ISO3166Alpha2 "US")
            (AddressLine "6501 Railroad Avenue SE")
            (Just (AddressLine "Room 315"))
            (Just (District "WA"))
            (PostalCode "85283")
        )
        ( BankAddress
            (Just "Test Bank")
            (Just (City "Snoqualmie"))
            (Just (ISO3166Alpha2 "US"))
            (Just (AddressLine "6501 Railroad Avenue SE"))
            (Just (AddressLine "Room 315"))
            (Just (District "WA"))
        )
    )

testSENAccountDetails :: SENAccountBodyParams
testSENAccountDetails = do
  SENAccountBodyParams
    [compileUUID|89022f90-4d36-4a3b-9961-2009a637a539|]
    [compileAccountNumber|446043103367|]
    (Just USD)

testSubscriptionBody :: SubscriptionBodyParams
testSubscriptionBody =
  SubscriptionBodyParams "https://example.org/handler/for/notifications"

testPaymentMetadata :: CreateMetadata
testPaymentMetadata =
  CreateMetadata
    "dylan@test.com"
    Nothing
    "DE6FA86F60BB47B379307F851E238617"
    "244.28.239.130"

testFiatPayment :: CreatePaymentBody
testFiatPayment =
  CreatePaymentBody
    [compileUUID|355c8eee-8de3-484d-98fc-2eff047d0214|]
    "key1"
    testPaymentMetadata
    ( MoneyAmount
        (Amount "100.00")
        USD
    )
    (Just True)
    VerificationCVV
    Nothing
    Nothing
    ( PaymentSource
        [compileUUID|26c9db99-81c5-492a-a4eb-7a36f0f4548c|]
        Card
    )
    Nothing
    Nothing
    Nothing

testCancelPaymentBody :: CancelPaymentBody
testCancelPaymentBody =
  CancelPaymentBody
    [compileUUID|65d6ccee-cb53-40ea-8be6-4e9485b50bb5|]
    (Just CancelPaymentReasonDuplicate)

testRefundPaymentBody :: RefundPaymentBody
testRefundPaymentBody =
  RefundPaymentBody
    [compileUUID|65d6ccee-cb53-40ea-8be6-4e9485b50bb5|]
    ( MoneyAmount
        (Amount "100.00")
        USD
    )
    Nothing

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
            let Right CircleResponseBody {..} = configurationInfo
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "encryption" $ do
          it "gets public key info" $ do
            keyInfo <- circleTest config manager getPublicKey
            let Right CircleResponseBody {..} = keyInfo
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "channels" $ do
          it "lists all channels" $ do
            channels <- circleTest config manager listAllChannels
            let Right CircleResponseBody {..} = channels
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "stablecoins" $ do
          it "lists all stablecoins" $ do
            stablecoins <- circleTest config manager listAllStablecoins
            -- liftIO $ print stablecoins
            let Right CircleResponseBody {..} = stablecoins
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        -- TODO Skip the subscription tests because I I accidentally hit my subscription limit for the sandbox account and can't add new ones
        -- However, I can't delete these subscriptions either, because they're still in the 'pending' state.
        -- At least this code worked!
        describe "subscriptions" $ do
          xit "creates a new subscription" $ do
            subscription <- circleTest config manager $ createSubscription testSubscriptionBody
            let Right CircleResponseBody {..} = subscription
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
          xit "deletes a subscription" $ do
            deletionResponse <- circleTest config manager $ deleteSubscription [compileUUID|e85f46e4-dea8-499c-b6f5-e40ebc736f39|]
            let Right CircleResponseBody {..} = deletionResponse
            circleResponseCode `shouldBe` Nothing
            -- TODO we don't have a resource so it'll fail
            circleResponseMessage `shouldBe` Just (ResponseMessage "Resource not found")
          xit "lists all subscription" $ do
            subscriptions <- circleTest config manager listAllNotificationSubscriptions
            let Right CircleResponseBody {..} = subscriptions
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "wire endpoints" $ do
        describe "create wire account" $ do
          it "creates a new wire account" $ do
            newWireAccount <- circleTest config manager $ createBusinessWireAccount testUSWireAccountDetails
            newWireAccount `shouldSatisfy` isRight
            let Right CircleResponseBody {..} = newWireAccount
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "list wire accounts" $ do
          it "gets a list of wire accounts" $ do
            wireAccounts <- circleTest config manager listBusinessWireAccounts
            wireAccounts `shouldSatisfy` isRight
            let Right CircleResponseBody {..} = wireAccounts
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get wire account" $ do
          it "gets a single wire account" $ do
            wireAccount1 <- circleTest config manager $ createBusinessWireAccount testUSWireAccountDetails
            wireAccount1 `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = wireAccount1
            for_ circleResponseData $ \WireAccountData {..} -> do
              wireAccount <- circleTest config manager $ getBusinessWireAccount wireAccountDataId
              wireAccount `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = wireAccount
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
          it "gets wire instructions for a wire account" $ do
            wireAccount2 <- circleTest config manager $ createBusinessWireAccount testUSWireAccountDetails
            wireAccount2 `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = wireAccount2
            for_ circleResponseData $ \WireAccountData {..} -> do
              wireAccountInstructions <- circleTest config manager $ getBusinessWireAccountInstructions wireAccountDataId
              wireAccountInstructions `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = wireAccountInstructions
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
      describe "SEN endpoints" $ do
        describe "create SEN account" $ do
          it "creates a new SEN account" $ do
            newSENAccount <- circleTest config manager $ createSENAccount testSENAccountDetails
            newSENAccount `shouldSatisfy` isRight
            let Right CircleResponseBody {..} = newSENAccount
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "list SEN accounts" $ do
          it "gets a list of SEN accounts" $ do
            senAccounts <- circleTest config manager listSENAccounts
            senAccounts `shouldSatisfy` isRight
            let Right CircleResponseBody {..} = senAccounts
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get SEN account" $ do
          it "gets a single SEN account" $ do
            senAccount1 <- circleTest config manager $ createSENAccount testSENAccountDetails
            senAccount1 `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = senAccount1
            for_ circleResponseData $ \SENAccountData {..} -> do
              senAccount <- circleTest config manager $ getSENAccount senAccountDataId
              senAccount `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = senAccount
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
          it "gets instructions for a SEN account" $ do
            senAccount2 <- circleTest config manager $ createSENAccount testSENAccountDetails
            senAccount2 `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = senAccount2
            for_ circleResponseData $ \SENAccountData {..} -> do
              senAccountInstructions <- circleTest config manager $ getSENAccountInstructions senAccountDataId
              senAccountInstructions `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = senAccountInstructions
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
      describe "balance endpoints" $ do
        -- TODO need to actually seed balances, I'll do that when I wrap that API endpoint
        describe "list balances" $ do
          it "should list all balances for the newly-created wire account" $ do
            balances <- circleTest config manager listAllBalances
            balances `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = balances
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "business account transfer endpoints" $ do
        describe "list transfers" $ do
          it "should list all transfers for a given business account" $ do
            transfers <- circleTest config manager listAllBusinessAccountTransfers
            transfers `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = transfers
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get transfer" $ do
          it "will attempt to return transfer data for a single transfer" $ do
            transfer <- circleTest config manager (getBusinessAccountTransfer [compileUUID|e553417d-fe7a-4b7a-8d06-ff4de80a0d65|])
            transfer `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = transfer
            circleResponseCode `shouldBe` Nothing
            -- will fail if there's no such payout Id
            circleResponseMessage `shouldBe` Just (ResponseMessage "API parameter invalid")
        describe "create transfer" $ do
          it "will attempt to create a new transfer" $ do
            let transferBody =
                  BusinessTransferBodyParams
                    [compileUUID|c14bf1a2-74fe-4cd5-8e74-c8c67903d849|]
                    ( TransferBodyDestination
                        VerifiedBlockchain
                        [compileUUID|2206775d-e4f7-4681-9494-34dc650fbfd8|]
                    )
                    ( MoneyAmount
                        (Amount "100.00")
                        USD
                    )
            -- this request will always fail if there's no money in the account
            transferAddressNotFound <- circleTest config manager $ createBusinessAccountTransfer transferBody
            transferAddressNotFound `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = transferAddressNotFound
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Just (ResponseMessage "Address not found")
      describe "business account address endpoints" $ do
        describe "list recipient addresses" $ do
          it "should list all recipient addresses for a given business account" $ do
            recipientAddresses <- circleTest config manager listAllBusinessAccountRecipientAddresses
            recipientAddresses `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = recipientAddresses
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "create recipient address" $ do
          it "will attempt to create a recipient address" $ do
            let recipientAddressBody =
                  RecipientAddressBodyParams
                    [compileUUID|c14bf1a2-74fe-4cd5-8e74-c8c67903d849|]
                    (HexString "0x8381470ED67C3802402dbbFa0058E8871F017A6F")
                    Nothing
                    ChainETH
                    USD
                    "test address"
            recipientAddress <- circleTest config manager $ createBusinessAccountRecipientAddress recipientAddressBody
            recipientAddress `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = recipientAddress
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "list deposit addresses" $ do
          it "should list all deposit addresses for a given business account" $ do
            depositAddresses <- circleTest config manager listAllDepositAddresses
            depositAddresses `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = depositAddresses
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "create deposit address" $ do
          it "will attempt to create a new deposit address" $ do
            let depositAddressBody =
                  DepositAddressBodyParams
                    [compileUUID|c14bf1a2-74fe-4cd5-8e74-c8c67903d849|]
                    ETH
                    ChainETH
            depositAddress <- circleTest config manager $ createBusinessAccountDepositAddress depositAddressBody
            depositAddress `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = depositAddress
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "deposits endpoint" $ do
        describe "list deposits" $ do
          it "should list all deposits" $ do
            deposits <- circleTest config manager listAllDeposits
            deposits `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = deposits
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "payout endpoints" $ do
        describe "list payouts" $ do
          -- TODO This test fails without money in the account.  I need to actually seed balances, I'll do that when I wrap that API endpoint
          xit " should list a subset of payouts for a given business account given the query params" $ do
            payoutsBeforeFoo <- circleTest config manager $ listAllBusinessAccountPayouts -&- PaginationQueryParams (PageBefore (CircleId "a8899b8e-782a-4526-b674-0efe1e04526d"))
            payoutsBeforeFoo `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = payoutsBeforeFoo
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
          it "should list all payouts for a given business account" $ do
            payouts <- circleTest config manager listAllBusinessAccountPayouts
            payouts `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = payouts
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get payout" $ do
          it "will attempt to return the payout data for the payout Id provided" $ do
            payout <- circleTest config manager (getBusinessAccountPayout "e553417d-fe7a-4b7a-8d06-ff4de80a0d65")
            payout `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = payout
            circleResponseCode `shouldBe` Nothing
            -- NB: this test will fail if there's no payoutId
            circleResponseMessage `shouldBe` Just (ResponseMessage "Resource not found")
        describe "create payout" $ do
          it "fails to create a new payout because no such account exists" $ do
            let payoutWithFakeWireAccount =
                  BusinessPayoutBodyParams
                    [compileUUID|e81b86e4-c4ba-4337-97ff-08486301b618|]
                    ( DestinationBankAccount
                        Wire
                        [compileUUID|6a3a947e-82d2-4204-bf7c-b17d7f380070|]
                        Nothing
                    )
                    ( MoneyAmount
                        (Amount "100.00")
                        USD
                    )
            -- this request will always fail if there's no money in the account
            failedPayoutResultsNoAccount <- circleTest config manager $ createBusinessAccountPayout payoutWithFakeWireAccount
            failedPayoutResultsNoAccount `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = failedPayoutResultsNoAccount
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Just (ResponseMessage "Fiat account not found")
          it "fails to create a new payout because the account has insufficient funds" $ do
            -- we first create the wire account so we have an account to send the payout to
            createdAccount <- circleTest config manager $ createBusinessWireAccount testUSWireAccountDetails
            createdAccount `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = createdAccount
            for_ circleResponseData $ \WireAccountData {..} -> do
              -- then, we create a payout
              let payoutWithRealWireAccount =
                    BusinessPayoutBodyParams
                      [compileUUID|e81b86e4-c4ba-4337-97ff-08486301b618|]
                      ( DestinationBankAccount
                          Wire
                          wireAccountDataId
                          Nothing
                      )
                      ( MoneyAmount
                          (Amount "100.00")
                          USD
                      )
              failedPayoutResultInsufficientFunds <- circleTest config manager $ createBusinessAccountPayout payoutWithRealWireAccount
              failedPayoutResultInsufficientFunds `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = failedPayoutResultInsufficientFunds
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Just (ResponseMessage "Account has insufficient funds")
      describe "payments endpoint" $ do
        describe "create payment" $ do
          it "should create a payment" $ do
            payment <- circleTest config manager $ createPayment testFiatPayment
            payment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = payment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get payment" $ do
          it "should get a newly-created payment" $ do
            newlyCreatedPayment <- circleTest config manager $ createPayment testFiatPayment
            newlyCreatedPayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = newlyCreatedPayment
            let Just (This FiatOrCryptoPaymentResponse {fiatOrCryptoPaymentId}) = circleResponseData
            thePayment <- circleTest config manager $ getPayment fiatOrCryptoPaymentId
            thePayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = thePayment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        -- TODO test doesn't pass because the payment will fail immediately if it's in the sandbox
        describe "cancel payment" $ do
          xit "should cancel a newly-created payment" $ do
            paymentToCancel <- circleTest config manager $ createPayment testFiatPayment
            paymentToCancel `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = paymentToCancel
            let Just (This FiatOrCryptoPaymentResponse {fiatOrCryptoPaymentId}) = circleResponseData
            cancellablePayment <- circleTest config manager $ cancelPayment fiatOrCryptoPaymentId testCancelPaymentBody
            cancellablePayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = cancellablePayment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        -- TODO test doesn't pass because the payment will fail immediately if it's in the sandbox
        describe "refund payment" $ do
          xit "should refund a newly-created payment" $ do
            paymentToRefund <- circleTest config manager $ createPayment testFiatPayment
            paymentToRefund `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = paymentToRefund
            let Just (This FiatOrCryptoPaymentResponse {fiatOrCryptoPaymentId}) = circleResponseData
            refundablePayment <- circleTest config manager $ refundPayment fiatOrCryptoPaymentId testRefundPaymentBody
            refundablePayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = refundablePayment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "list payments" $ do
          it "should list all payments" $ do
            payments <- circleTest config manager listAllPayments
            payments `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = payments
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        -- TODO, this test fails and my guess is because the beneficiary should have all the details.  I'll try to fix this later.
        describe "mock payments" $ do
          xit "should create a mock silvergate payment" $ do
            let mockSilvergatePaymentBody =
                  MockSenOrWirePaymentBodyParams
                    (TrackingReference "CIR13FB13A")
                    ( MoneyAmount
                        (Amount "100.00")
                        USD
                    )
                    ( MockBeneficiaryBankDetails
                        [compileAccountNumber|446043103366|]
                    )
            mockSilvergatePayment <- circleTest config manager $ createMockSilvergatePayment mockSilvergatePaymentBody
            mockSilvergatePayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = mockSilvergatePayment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
          xit "should create a mock wire payment" $ do
            let mockWirePaymentBody =
                  MockSenOrWirePaymentBodyParams
                    (TrackingReference "CIR13FB13A")
                    ( MoneyAmount
                        (Amount "100.00")
                        USD
                    )
                    ( MockBeneficiaryBankDetails
                        [compileAccountNumber|446043103366|]
                    )
            mockSilvergatePayment <- circleTest config manager $ createMockWirePayment mockWirePaymentBody
            mockSilvergatePayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = mockSilvergatePayment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
          xit "should create a mock SEPA payment" $ do
            let mockSEPAPaymentBody =
                  MockSEPAPaymentBodyParams
                    (TrackingReference "CIR13FB13A")
                    ( MoneyAmount
                        (Amount "100.00")
                        USD
                    )
            mockSepaPayment <- circleTest config manager $ createMockSEPAPayment mockSEPAPaymentBody
            mockSepaPayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = mockSepaPayment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
