{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec
import Test.Hspec.Expectations.Contrib (isRight)
import Unknot.Client
import Unknot.Types
import Data.Maybe (fromJust)

testUSWireAccountDetails :: UUID -> WireAccountRequestBody
testUSWireAccountDetails wireAccountIdempotencyKey =
  USBankAccount
      ( USBankAccountRequestBody
          wireAccountIdempotencyKey
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

testBusinessTransferRequestBody :: UUID -> UUID -> BusinessTransferRequestBody
testBusinessTransferRequestBody businessTransferRequestIdempotencyKey transferDestinationAddress =
  BusinessTransferRequestBody
    businessTransferRequestIdempotencyKey
    ( TransferDestination
        VerifiedBlockchain
        transferDestinationAddress
    )
    ( MoneyAmount
        (Amount "100.00")
        USD
    )

testOnChainTransferRequestBody :: UUID -> SourceWallet -> DestinationWallet -> OnChainTransferRequestBody
testOnChainTransferRequestBody onChainTransferIdempotencyKey sourceWallet destinationWallet =
  OnChainTransferRequestBody
    onChainTransferIdempotencyKey
    sourceWallet
    (This destinationWallet)
    ( MoneyAmount
        (Amount "100.00")
        USD
    )

testRecipientAddressRequestBody :: UUID -> RecipientAddressRequestBody
testRecipientAddressRequestBody recipientAddressIdempotencyKey =
  RecipientAddressRequestBody
    recipientAddressIdempotencyKey
    -- TODO will need to make this random each time
    (HexString "0x8381470ED67C3802402dbbFa0058E8871F017A6K")
    Nothing
    ChainETH
    USD
    "test address"

testDepositAddressRequestBody :: UUID -> DepositAddressRequestBody
testDepositAddressRequestBody depositAddressIdempotencyKey =
  DepositAddressRequestBody
    depositAddressIdempotencyKey
    USD
    ChainETH

testSENAccountDetails :: UUID -> SENAccountRequestBody
testSENAccountDetails senAccountIdempotencyKey =
  SENAccountRequestBody
      senAccountIdempotencyKey
      [compileAccountNumber|446043103367|]
      (Just USD)

testSubscriptionBody :: SubscriptionRequestBody
testSubscriptionBody =
  SubscriptionRequestBody "https://example.org/handler/for/notifications"

testPaymentMetadata :: RequestMetadata
testPaymentMetadata =
  RequestMetadata
    [compileEmail|dylan@test.com|]
    Nothing
    ( SessionId "DE6FA86F60BB47B379307F851E238617" )
    ( IPAddress "244.28.239.130" )

testCreateCardRequestBody :: UUID -> RequestMetadata -> CreateCardRequestBody
testCreateCardRequestBody cardIdempotencyKeyUUID = CreateCardRequestBody
    cardIdempotencyKeyUUID
    (Just "key1")
    (Just "LS0tLS1CRUdJTiBQR1AgTUVTU0FHRS0tLS0tCgp3Y0JNQTBYV1NGbEZScFZoQVFmL2J2bVVkNG5LZ3dkbExKVTlEdEFEK0p5c0VOTUxuOUlRUWVGWnZJUWEKMGgzQklpRFNRU0RMZmI0NEs2SXZMeTZRbm54bmFLcWx0MjNUSmtPd2hGWFIrdnNSMU5IbnVHN0lUNWJECmZzeVdleXlNK1JLNUVHV0thZ3NmQ2tWamh2NGloY29xUnlTTGtJbWVmRzVaR0tMRkJTTTBsTFNPWFRURQpiMy91eU1zMVJNb3ZiclNvbXkxa3BybzUveWxabWVtV2ZsU1pWQlhNcTc1dGc1YjVSRVIraXM5ckc0cS8KMXl0M0FOYXA3UDhKekFhZVlyTnVNZGhGZFhvK0NFMC9CQnN3L0NIZXdhTDk4SmRVUEV0NjA5WFRHTG9kCjZtamY0YUtMQ01xd0RFMkNVb3dPdE8vMzVIMitnVDZKS3FoMmtjQUQyaXFlb3luNWcralRHaFNyd3NKWgpIdEphQWVZZXpGQUVOaFo3Q01IOGNsdnhZVWNORnJuNXlMRXVGTkwwZkczZy95S3loclhxQ0o3UFo5b3UKMFVxQjkzQURKWDlJZjRBeVQ2bU9MZm9wUytpT2lLall4bG1NLzhlVWc3OGp1OVJ5T1BXelhyTzdLWTNHClFSWm8KPXc1dEYKLS0tLS1FTkQgUEdQIE1FU1NBR0UtLS0tLQo")
    ( BillingDetails
        "Test Recipient"
        (City "Snoqualmie")
        (ISO3166Alpha2 "US")
        (AddressLine "6501 Railroad Avenue SE")
        (Just (AddressLine "Room 315"))
        (Just (District "WA"))
        (PostalCode "85283")
    )
    12
    2027

testFiatPayment :: UUID -> UUID -> CreatePaymentRequestBody
testFiatPayment paymentRequestIdempotencyKey paymentSourceId =
  CreatePaymentRequestBody
      paymentRequestIdempotencyKey
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
          paymentSourceId
          Card
      )
      Nothing
      Nothing
      Nothing

testCancelPaymentBody :: UUID -> CancelPaymentRequestBody
testCancelPaymentBody cancelPaymentIdempotencyKey =
  CancelPaymentRequestBody
      cancelPaymentIdempotencyKey
      (Just CancelPaymentReasonDuplicate)

testRefundPaymentBody :: UUID -> RefundPaymentRequestBody
testRefundPaymentBody refundPaymentIdempotencyKey =
  RefundPaymentRequestBody
      refundPaymentIdempotencyKey
      ( MoneyAmount
          (Amount "100.00")
          USD
      )
      Nothing

testBusinessPayoutRequestBody :: UUID -> UUID -> BusinessPayoutRequestBody
testBusinessPayoutRequestBody payoutIdempotencyKey wireAccountId =
  BusinessPayoutRequestBody
    payoutIdempotencyKey
    ( DestinationBankAccount
        Wire
        wireAccountId
        Nothing
    )
    ( MoneyAmount
        (Amount "100.00")
        USD
    )

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  config <- sandboxEnvConfig
  hspec $
    parallel $ do
      describe "/configuration endpoint" $ do
        describe "management" $ do
          it "gets configuration info" $ do
            configurationInfo <- circleTest config manager getConfigurationInfo
            let Right CircleResponseBody {..} = configurationInfo
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "/encryption/public" $ do
          it "gets public key info" $ do
            keyInfo <- circleTest config manager getPublicKey
            let Right CircleResponseBody {..} = keyInfo
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "/channels" $ do
          it "lists all channels" $ do
            channels <- circleTest config manager listAllChannels
            let Right CircleResponseBody {..} = channels
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "/stablecoins" $ do
          it "lists all stablecoins" $ do
            stablecoins <- circleTest config manager listAllStablecoins
            -- liftIO $ print stablecoins
            let Right CircleResponseBody {..} = stablecoins
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        -- TODO Skip the subscription tests because I I accidentally hit my subscription limit for the sandbox account and can't add new ones
        -- However, I can't delete these subscriptions either, because they're still in the 'pending' state.
        -- At least this code worked!
        describe "/notifications/subscriptions" $ do
          xit "creates a new subscription" $ do
            subscription <- circleTest config manager $ createSubscription testSubscriptionBody
            let Right CircleResponseBody {..} = subscription
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
          xit "deletes a subscription" $ do
            -- TODO replace this with an actual value
            randomSubscriptionId <- liftIO $ UUID.nextRandom -- IO UUID
            deletionResponse <- circleTest config manager $ deleteSubscription randomSubscriptionId
            let Right CircleResponseBody {..} = deletionResponse
            circleResponseCode `shouldBe` Nothing
            -- TODO we don't have a resource so it'll fail
            circleResponseMessage `shouldBe` Just (ResponseMessage "Resource not found")
          xit "lists all subscription" $ do
            subscriptions <- circleTest config manager listAllNotificationSubscriptions
            let Right CircleResponseBody {..} = subscriptions
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "/businessAccount/banks/wires endpoint" $ do
        describe "create business wire account" $ do
          it "creates a new business wire account" $ do
            wireAccountIdempotencyKey <- liftIO $ UUID.nextRandom
            newWireAccount <- circleTest config manager $ createBusinessWireAccount (testUSWireAccountDetails wireAccountIdempotencyKey)
            newWireAccount `shouldSatisfy` isRight
            let Right CircleResponseBody {..} = newWireAccount
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "list business wire accounts" $ do
          it "gets a list of business wire accounts" $ do
            wireAccounts <- circleTest config manager listBusinessWireAccounts
            wireAccounts `shouldSatisfy` isRight
            let Right CircleResponseBody {..} = wireAccounts
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get business wire account" $ do
          it "gets a single business wire account" $ do
            wireAccountIdempotencyKey <- liftIO $ UUID.nextRandom
            wireAccount1 <- circleTest config manager $ createBusinessWireAccount (testUSWireAccountDetails wireAccountIdempotencyKey)
            wireAccount1 `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = wireAccount1
            for_ circleResponseData $ \WireAccountResponseBody {..} -> do
              wireAccount <- circleTest config manager $ getBusinessWireAccount wireAccountResponseBodyId
              wireAccount `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = wireAccount
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
          it "gets wire instructions for a business wire account" $ do
            wireAccountIdempotencyKey <- liftIO $ UUID.nextRandom
            wireAccount2 <- circleTest config manager $ createBusinessWireAccount (testUSWireAccountDetails wireAccountIdempotencyKey)
            wireAccount2 `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = wireAccount2
            for_ circleResponseData $ \WireAccountResponseBody {..} -> do
              wireAccountInstructions <- circleTest config manager $ getBusinessWireAccountInstructions wireAccountResponseBodyId
              wireAccountInstructions `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = wireAccountInstructions
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
      describe "/businessAccount/banks/sen endpoint" $ do
        describe "create SEN account" $ do
          it "creates a new SEN account" $ do
            senAccountIdempotencyKey <- liftIO $ UUID.nextRandom
            newSENAccount <- circleTest config manager $ createSENAccount (testSENAccountDetails senAccountIdempotencyKey)
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
            senAccountIdempotencyKey <- liftIO $ UUID.nextRandom
            senAccount1 <- circleTest config manager $ createSENAccount (testSENAccountDetails senAccountIdempotencyKey)
            senAccount1 `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = senAccount1
            for_ circleResponseData $ \SENAccountResponseBody {..} -> do
              senAccount <- circleTest config manager $ getSENAccount senAccountResponseBodyId
              senAccount `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = senAccount
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
          it "gets instructions for a SEN account" $ do
            senAccountIdempotencyKey <- liftIO $ UUID.nextRandom
            senAccount2 <- circleTest config manager $ createSENAccount (testSENAccountDetails senAccountIdempotencyKey)
            senAccount2 `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = senAccount2
            for_ circleResponseData $ \SENAccountResponseBody {..} -> do
              senAccountInstructions <- circleTest config manager $ getSENAccountInstructions senAccountResponseBodyId
              senAccountInstructions `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = senAccountInstructions
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Nothing
      describe "/balances endpoint" $ do
        -- TODO need to actually seed balances, I'll do that when I wrap that API endpoint
        describe "list balances" $ do
          it "should list all balances for the newly-created wire account" $ do
            balances <- circleTest config manager listAllBalances
            balances `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = balances
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "/businessAccount/transfers endpoint" $ do
        describe "list transfers" $ do
          it "should list all transfers for a given business account" $ do
            transfers <- circleTest config manager listAllBusinessAccountTransfers
            transfers `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = transfers
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get transfer" $ do
          it "will attempt to return transfer data for a single transfer" $ do
            uuid <- liftIO UUID.nextRandom
            transfer <- circleTest config manager (getBusinessAccountTransfer uuid)
            transfer `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = transfer
            circleResponseCode `shouldBe` Nothing
            -- will fail if there's no such payout Id
            circleResponseMessage `shouldBe` Just (ResponseMessage "API parameter invalid")
        describe "create transfer" $ do
          it "will attempt to create a new transfer" $ do
            businessTransferRequestIdempotencyKey <- liftIO $ UUID.nextRandom
            transferDestinationAddress <- liftIO $ UUID.nextRandom
            -- this request will always fail if there's no money in the account
            transferAddressNotFound <- circleTest config manager $ createBusinessAccountTransfer (testBusinessTransferRequestBody businessTransferRequestIdempotencyKey transferDestinationAddress)
            transferAddressNotFound `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = transferAddressNotFound
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Just (ResponseMessage "Address not found")
      describe "/businessAccount/wallets/addresses endpoint" $ do
        describe "list recipient addresses" $ do
          it "should list all recipient addresses for a given business account" $ do
            recipientAddresses <- circleTest config manager listAllBusinessAccountRecipientAddresses
            recipientAddresses `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = recipientAddresses
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "create recipient address" $ do
          it "will attempt to create a recipient address" $ do
            recipientAddressIdempotencyKey <- liftIO $ UUID.nextRandom
            recipientAddress <- circleTest config manager $ createBusinessAccountRecipientAddress (testRecipientAddressRequestBody recipientAddressIdempotencyKey)
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
            let depositAddressIdempotencyKey = fromJust $ UUID.fromText "ba943ff1-ca16-49b2-ba55-1057e70ca5c7"
            depositAddress <- circleTest config manager $ createBusinessAccountDepositAddress (testDepositAddressRequestBody depositAddressIdempotencyKey)
            depositAddress `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = depositAddress
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "/businessAccount/deposits endpoint" $ do
        describe "list deposits" $ do
          it "should list all deposits" $ do
            deposits <- circleTest config manager listAllDeposits
            deposits `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = deposits
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
      describe "/businessAccount/payouts endpoint" $ do
        describe "list payouts" $ do
          -- TODO This test fails without money in the account.  I need to actually seed balances, I'll do that when I wrap that API endpoint
          xit " should list a subset of payouts for a given business account given the query params" $ do
            payoutsBeforeFoo <- circleTest config manager $ listAllBusinessAccountPayouts -&- PaginationQueryParams (PageBefore "a8899b8e-782a-4526-b674-0efe1e04526d")
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
            fakePayoutId <- liftIO $ UUID.nextRandom
            failedPayout <- circleTest config manager (getBusinessAccountPayout fakePayoutId)
            failedPayout `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = failedPayout
            circleResponseCode `shouldBe` Nothing
            -- NB: this test will fail if there's no payoutId
            circleResponseMessage `shouldBe` Just (ResponseMessage "Resource not found")
        describe "create payout" $ do
          it "fails to create a new payout because no such account exists" $ do
            businessPayoutIdempotencyKey <- liftIO $ UUID.nextRandom
            destinationAccountId <- liftIO $ UUID.nextRandom
            failedPayoutResultsNoAccount <- circleTest config manager $ createBusinessAccountPayout (testBusinessPayoutRequestBody businessPayoutIdempotencyKey destinationAccountId)
            failedPayoutResultsNoAccount `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = failedPayoutResultsNoAccount
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Just (ResponseMessage "Fiat account not found")
          it "fails to create a new payout because the account has insufficient funds" $ do
            -- we first create the wire account so we have an account to send the payout to
            wireAccountIdempotencyKey <- liftIO $ UUID.nextRandom
            createdAccount <- circleTest config manager $ createBusinessWireAccount (testUSWireAccountDetails wireAccountIdempotencyKey)
            createdAccount `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = createdAccount
            for_ circleResponseData $ \WireAccountResponseBody {..} -> do
              -- then, we create a payout
              payoutIdempotencyKey <- liftIO $ UUID.nextRandom
              -- this request will fail because there is no money in the account
              failedPayoutResultInsufficientFunds <- circleTest config manager $ createBusinessAccountPayout (testBusinessPayoutRequestBody payoutIdempotencyKey wireAccountResponseBodyId)
              failedPayoutResultInsufficientFunds `shouldSatisfy` isRight
              let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = failedPayoutResultInsufficientFunds
              circleResponseCode `shouldBe` Nothing
              circleResponseMessage `shouldBe` Just (ResponseMessage "Account has insufficient funds")
      describe "/payments endpoint" $ do
        describe "create payment" $ do
          it "should create a payment" $ do
            paymentRequestIdempotencyKey <- liftIO $ UUID.nextRandom
            paymentSourceId <- liftIO $ UUID.nextRandom
            payment <- circleTest config manager $ createPayment (testFiatPayment paymentRequestIdempotencyKey paymentSourceId)
            payment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = payment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get payment" $ do
          it "should get a newly-created payment" $ do
            paymentRequestIdempotencyKey <- liftIO $ UUID.nextRandom
            paymentSourceId <- liftIO $ UUID.nextRandom
            newlyCreatedPayment <- circleTest config manager $ createPayment (testFiatPayment paymentRequestIdempotencyKey paymentSourceId)
            newlyCreatedPayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = newlyCreatedPayment
            let Just (This FiatOrCryptoPaymentResponseBody {fiatOrCryptoPaymentId}) = circleResponseData
            thePayment <- circleTest config manager $ getPayment fiatOrCryptoPaymentId
            thePayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = thePayment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        -- TODO test doesn't pass because the payment will fail immediately if it's in the sandbox
        describe "cancel payment" $ do
          xit "should cancel a newly-created payment" $ do
            paymentRequestIdempotencyKey <- liftIO $ UUID.nextRandom
            paymentSourceId <- liftIO $ UUID.nextRandom
            paymentToCancel <- circleTest config manager $ createPayment (testFiatPayment paymentRequestIdempotencyKey paymentSourceId)
            paymentToCancel `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = paymentToCancel
            let Just (This FiatOrCryptoPaymentResponseBody {fiatOrCryptoPaymentId}) = circleResponseData
            cancelPaymentIdempotencyKey <- liftIO $ UUID.nextRandom
            cancellablePayment <- circleTest config manager $ cancelPayment fiatOrCryptoPaymentId (testCancelPaymentBody cancelPaymentIdempotencyKey)
            cancellablePayment `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = cancellablePayment
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        -- TODO test doesn't pass because the payment will fail immediately if it's in the sandbox
        describe "refund payment" $ do
          xit "should refund a newly-created payment" $ do
            paymentRequestIdempotencyKey <- liftIO $ UUID.nextRandom
            paymentSourceId <- liftIO $ UUID.nextRandom
            paymentToRefund <- circleTest config manager $ createPayment (testFiatPayment paymentRequestIdempotencyKey paymentSourceId)
            paymentToRefund `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseData} = paymentToRefund
            let Just (This FiatOrCryptoPaymentResponseBody {fiatOrCryptoPaymentId}) = circleResponseData
            refundPaymentIdempotencyKey <- liftIO $ UUID.nextRandom
            refundablePayment <- circleTest config manager $ refundPayment fiatOrCryptoPaymentId (testRefundPaymentBody refundPaymentIdempotencyKey)
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
                  MockSenOrWirePaymentRequestBody
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
                  MockSenOrWirePaymentRequestBody
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
                  MockSEPAPaymentRequestBody
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
      describe "/transfers endpoint" $ do
        describe "create an on-chain transfer" $ do
          it "should create an on-chain transfers" $ do
            onChainTransferIdempotencyKey <- liftIO $ UUID.nextRandom
            let identity =
                  Identity
                    Individual
                    "Mario"
                    [ ( Address
                          (Just (City "Snoqualmie"))
                          (Just (ISO3166Alpha2 "US"))
                          (Just (AddressLine "6501 Railroad Avenue SE"))
                          (Just (AddressLine "Room 315"))
                          (Just (District "WA"))
                      )
                    ]
                sourceWallet =
                  SourceWallet
                    Wallet
                    (WalletId "987654321")
                    [identity]
                destinationWallet =
                  DestinationWallet
                    Wallet
                    (WalletId "123456789")
                    Nothing
                    Nothing
            newOnChainTransfer <- circleTest config manager $ createOnChainTransfer (testOnChainTransferRequestBody onChainTransferIdempotencyKey sourceWallet destinationWallet)
            newOnChainTransfer `shouldSatisfy` isRight
        -- TODO this is broken
        -- let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = onChainTransfer
        -- circleResponseCode `shouldBe` Nothing
        -- circleResponseMessage `shouldBe` Nothing
        describe "list on-chain transfers" $ do
          it "should list all on-chain transfers" $ do
            onChainTransfers <- circleTest config manager listAllOnChainTransfers
            onChainTransfers `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = onChainTransfers
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "get an on-chain transfer" $ do
          it "should get an on-chain transfers" $ do
            uuid <- liftIO $ UUID.nextRandom
            onChainTransfer <- circleTest config manager $ getOnChainTransfer uuid
            onChainTransfer `shouldSatisfy` isRight
      -- TODO this is broken
      -- let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = onChainTransfer
      -- circleResponseCode `shouldBe` Nothing
      -- circleResponseMessage `shouldBe` Nothing
      describe "/cards endpoint" $ do
        describe "list cards" $ do
          it "should list all cards" $ do
            cards <- circleTest config manager listAllCards
            cards `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage} = cards
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
        describe "create card" $ do
          it "should create a new card, get it, and then update it" $ do
            cardIdempotencyKeyUUID <- liftIO $ UUID.nextRandom
            let testUpdateCardBody =
                  UpdateCardRequestBody
                    (Just "key1")
                    (Just "LS0tLS1CRUdJTiBQR1AgTUVTU0FHRS0tLS0tCgp3Y0JNQTBYV1NGbEZScFZoQVFmL2J2bVVkNG5LZ3dkbExKVTlEdEFEK0p5c0VOTUxuOUlRUWVGWnZJUWEKMGgzQklpRFNRU0RMZmI0NEs2SXZMeTZRbm54bmFLcWx0MjNUSmtPd2hGWFIrdnNSMU5IbnVHN0lUNWJECmZzeVdleXlNK1JLNUVHV0thZ3NmQ2tWamh2NGloY29xUnlTTGtJbWVmRzVaR0tMRkJTTTBsTFNPWFRURQpiMy91eU1zMVJNb3ZiclNvbXkxa3BybzUveWxabWVtV2ZsU1pWQlhNcTc1dGc1YjVSRVIraXM5ckc0cS8KMXl0M0FOYXA3UDhKekFhZVlyTnVNZGhGZFhvK0NFMC9CQnN3L0NIZXdhTDk4SmRVUEV0NjA5WFRHTG9kCjZtamY0YUtMQ01xd0RFMkNVb3dPdE8vMzVIMitnVDZKS3FoMmtjQUQyaXFlb3luNWcralRHaFNyd3NKWgpIdEphQWVZZXpGQUVOaFo3Q01IOGNsdnhZVWNORnJuNXlMRXVGTkwwZkczZy95S3loclhxQ0o3UFo5b3UKMFVxQjkzQURKWDlJZjRBeVQ2bU9MZm9wUytpT2lLall4bG1NLzhlVWc3OGp1OVJ5T1BXelhyTzdLWTNHClFSWm8KPXc1dEYKLS0tLS1FTkQgUEdQIE1FU1NBR0UtLS0tLQo")
                    1
                    2028
            newCard <- circleTest config manager $ createCard (testCreateCardRequestBody cardIdempotencyKeyUUID testPaymentMetadata)
            newCard `shouldSatisfy` isRight
            let Right CircleResponseBody {circleResponseCode, circleResponseMessage, circleResponseData} = newCard
            circleResponseCode `shouldBe` Nothing
            circleResponseMessage `shouldBe` Nothing
            for_ circleResponseData $ \CardResponseBody {..} -> do
              -- get the card
              card <- circleTest config manager $ getCard cardId
              card `shouldSatisfy` isRight
              -- update the card
              updatedCard <- circleTest config manager $ updateCard cardId testUpdateCardBody
              updatedCard `shouldSatisfy` isRight
