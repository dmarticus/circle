{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Unknot.Client where

-- import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import Network.HTTP.Client
  ( Manager,
    Request (method, queryString, requestBody),
    RequestBody (RequestBodyLBS),
    Response (responseBody),
    applyBearerAuth,
    httpLbs,
    newManager,
    parseRequest,
    requestHeaders,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentType, hAccept)
import qualified Network.HTTP.Types.Method as NHTM
import Unknot.Types

---------------------------------------------------------------
-- Wire endpoints
---------------------------------------------------------------
-- | Create a bank account for a wire
-- https://developers.circle.com/reference/createbusinesswireaccount
createWireAccount :: WireAccountBodyParams -> CircleAPIRequest WireAccountRequest TupleBS8 BSL.ByteString
createWireAccount wireAccountBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/banks/wires"
    params = Params (Just $ Body (encode wireAccountBody)) []

-- | Get a list of wire accounts
-- https://developers.circle.com/reference/listbusinesswireaccounts
listWireAccounts :: CircleAPIRequest WireAccountsRequest TupleBS8 BSL.ByteString
listWireAccounts = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/wires"
    params = Params Nothing []

-- | Get a single wire account, accepts the wire account Id as a parameter
-- https://developers.circle.com/reference/getbusinesswireaccount
getWireAccount :: UUID -> CircleAPIRequest WireAccountRequest TupleBS8 BSL.ByteString
getWireAccount wireAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/wires/" <> unUUID wireAccountId
    params = Params Nothing []

-- | Get the wire transfer instructions into the Circle bank account given your bank account id.
-- https://developers.circle.com/reference/getbusinesswireaccountinstructions
getWireAccountInstructions :: UUID -> CircleAPIRequest WireInstructionsRequest TupleBS8 BSL.ByteString
getWireAccountInstructions wireAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/wires/" <> unUUID wireAccountId <> "/instructions"
    params = Params Nothing []

---------------------------------------------------------------
-- Balance endpoints
---------------------------------------------------------------
-- | List all balances
-- https://developers.circle.com/reference/listbusinesspayouts
listAllBalances :: CircleAPIRequest BalanceRequest TupleBS8 BSL.ByteString
listAllBalances = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/balances"
    params = Params Nothing []

---------------------------------------------------------------
-- Management endpoint
---------------------------------------------------------------
-- | Get configuration info
-- https://developers.circle.com/reference/getaccountconfig
getConfigurationInfo :: CircleAPIRequest ConfigurationRequest TupleBS8 BSL.ByteString
getConfigurationInfo = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "configuration"
    params = Params Nothing []

---------------------------------------------------------------
-- Encryption endpoint
---------------------------------------------------------------
-- | Get encryption info
-- https://developers.circle.com/reference/getpublickey
getPublicKey :: CircleAPIRequest EncryptionRequest TupleBS8 BSL.ByteString
getPublicKey = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "encryption/public"
    params = Params Nothing []

---------------------------------------------------------------
-- Channels endpoint
---------------------------------------------------------------
-- | List all channels
-- https://developers.circle.com/reference/listchannels
listAllChannels :: CircleAPIRequest ChannelsRequest TupleBS8 BSL.ByteString
listAllChannels = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "channels"
    params = Params Nothing []

---------------------------------------------------------------
-- Stablecoins endpoint
---------------------------------------------------------------
-- | List all stablecoins
-- https://developers.circle.com/reference/listchannels
listAllStablecoins :: CircleAPIRequest StablecoinsRequest TupleBS8 BSL.ByteString
listAllStablecoins = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "stablecoins"
    params = Params Nothing []

---------------------------------------------------------------
-- Subscriptions endpoints
---------------------------------------------------------------
-- | List all subscriptions
-- https://developers.circle.com/reference/listsubscriptions
listAllNotificationSubscriptions :: CircleAPIRequest SubscriptionsRequest TupleBS8 BSL.ByteString
listAllNotificationSubscriptions = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "notifications/subscriptions"
    params = Params Nothing []

-- | Create new subscription
-- https://developers.circle.com/reference/createsubscribtion
createSubscription :: SubscriptionBodyParams -> CircleAPIRequest SubscriptionRequest TupleBS8 BSL.ByteString
createSubscription subscriptionBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "notifications/subscriptions"
    params = Params (Just $ Body (encode subscriptionBody)) []

-- | Delete subscription
-- https://developers.circle.com/reference/deletesubscribtion
deleteSubscription :: UUID -> CircleAPIRequest SubscriptionsRequest TupleBS8 BSL.ByteString
deleteSubscription resourceId = do
  mkCircleAPIRequest NHTM.methodDelete url params
  where
    url = "notifications/subscriptions" <> unUUID resourceId
    params = Params Nothing []

---------------------------------------------------------------
-- Payout endpoints
---------------------------------------------------------------

-- | Lists all payouts made from a given business accounts
-- https://developers.circle.com/reference/listbusinesspayouts
listAllPayouts :: CircleAPIRequest PayoutsRequest TupleBS8 BSL.ByteString
listAllPayouts = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/payouts"
    params = Params Nothing []

-- | Gets a specific payout based on Id
-- https://developers.circle.com/reference/getbusinesspayout
getPayout :: T.Text -> CircleAPIRequest PayoutRequest TupleBS8 BSL.ByteString
getPayout payoutId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/payouts" <> payoutId
    params = Params Nothing []

-- | Creates a payout
-- https://developers.circle.com/reference/createbusinesspayout
createPayout :: PayoutBodyParams -> CircleAPIRequest PayoutRequest TupleBS8 BSL.ByteString
createPayout payoutBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/payouts"
    params = Params (Just $ Body (encode payoutBody)) []

---------------------------------------------------------------
-- Transfer endpoints
---------------------------------------------------------------

-- | Searches for transfers from your business account. 
-- If the date parameters are omitted, returns the most recent transfers. 
-- This endpoint returns up to 50 transfers in descending chronological order or pageSize, if provided.
-- https://developers.circle.com/reference/listbusinesstransfers
listAllTransfers :: CircleAPIRequest TransfersRequest TupleBS8 BSL.ByteString
listAllTransfers = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/transfers"
    params = Params Nothing []

-- | Get a transfer based on a transfer ID
-- https://developers.circle.com/reference/getbusinesstransfer
getTransfer :: T.Text -> CircleAPIRequest TransferRequest TupleBS8 BSL.ByteString
getTransfer transferId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/transfers" <> transferId
    params = Params Nothing []

-- | Create a new transfer
-- https://developers.circle.com/reference/createbusinesstransfer
createTransfer :: TransferBodyParams -> CircleAPIRequest TransferRequest TupleBS8 BSL.ByteString
createTransfer transferBody = do
    mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/transfers"
    params = Params (Just $ Body (encode transferBody)) []

---------------------------------------------------------------
-- Address endpoints
---------------------------------------------------------------

-- | List all deposit addresses
-- https://developers.circle.com/developer/reference/getbusinessdepositaddress
listAllDepositAddresses :: CircleAPIRequest DepositAddressesRequest TupleBS8 BSL.ByteString
listAllDepositAddresses = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/wallets/addresses/deposit"
    params = Params Nothing []

-- | Create new deposit address
-- Generates a new blockchain address for a wallet for a given currency/chain pair. 
-- Circle may reuse addresses on blockchains that support reuse. 
-- For example, if you're requesting two addresses for depositing USD and ETH, both on Ethereum, 
-- you may see the same Ethereum address returned. 
-- Depositing cryptocurrency to a generated address will credit the associated wallet with the value of the deposit.
-- https://developers.circle.com/developer/reference/createbusinessdepositaddress
createDepositAddress :: DepositAddressBodyParams -> CircleAPIRequest DepositAddressRequest TupleBS8 BSL.ByteString
createDepositAddress depositAddressBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/wallets/addresses/deposit"
    params = Params (Just $ Body (encode depositAddressBody)) []

-- | List all recipient addresses
-- Returns a list of recipient addresses that have each been verified and are eligible for transfers. 
-- Any recipient addresses pending verification are not included in the response.
-- https://developers.circle.com/developer/reference/listbusinessrecipientaddresses
listAllRecipientAddresses :: CircleAPIRequest RecipientAddressesRequest TupleBS8 BSL.ByteString
listAllRecipientAddresses = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/wallets/addresses/recipient"
    params = Params Nothing []

-- | Create a new recipient address
-- Stores an external blockchain address. Once added, the recipient address must be verified to ensure that you know and trust each new address.
-- https://developers.circle.com/developer/reference/createbusinessrecipientaddress
createRecipientAddress :: RecipientAddressBodyParams -> CircleAPIRequest RecipientAddressRequest TupleBS8 BSL.ByteString
createRecipientAddress recipientAddressBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/wallets/addresses/recipient"
    params = Params (Just $ Body (encode recipientAddressBody)) []

---------------------------------------------------------------
-- Deposits Endpoint
---------------------------------------------------------------

-- | List all deposits
-- Searches for deposits sent to your business account. If the date parameters are omitted, returns the most recent deposits. 
-- This endpoint returns up to 50 deposits in descending chronological order or pageSize, if provided.
-- https://developers.circle.com/developer/reference/listbusinessdeposits
listAllDeposits :: CircleAPIRequest DepositsRequest TupleBS8 BSL.ByteString
listAllDeposits = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/deposits"
    params = Params Nothing []

---------------------------------------------------------------
-- Silvergate SEN Endpoints
---------------------------------------------------------------

-- | Create a bank account for a SEN
-- https://developers.circle.com/developer/reference/createbusinesssenaccount
createSENAccount :: SENAccountBodyParams -> CircleAPIRequest SENAccountRequest TupleBS8 BSL.ByteString
createSENAccount senAccountBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/banks/sen"
    params = Params (Just $ Body (encode senAccountBody)) []

-- | Get a list of SEN accounts
-- https://developers.circle.com/developer/reference/listbusinesssenaccounts
listSENAccounts :: CircleAPIRequest SENAccountsRequest TupleBS8 BSL.ByteString
listSENAccounts = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/sen"
    params = Params Nothing []

-- | Get a single SEN account, accepts the SEN account Id as a parameter
-- https://developers.circle.com/developer/reference/getbusinesssenaccount
getSENAccount :: UUID -> CircleAPIRequest SENAccountRequest TupleBS8 BSL.ByteString
getSENAccount senAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/sen/" <> unUUID senAccountId
    params = Params Nothing []

-- | Get the SEN transfer instructions into the Circle bank account given your bank account id.
-- https://developers.circle.com/developer/reference/getbusinesssenaccountinstructions
getSENAccountInstructions :: UUID -> CircleAPIRequest SENInstructionsRequest TupleBS8 BSL.ByteString
getSENAccountInstructions senAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/sen/" <> unUUID senAccountId <> "/instructions"
    params = Params Nothing []

---------------------------------------------------------------
-- Payment Endpoints
---------------------------------------------------------------

-- | List all payments
-- https://developers.circle.com/developer/reference/listpayments
listAllPayments :: CircleAPIRequest PaymentsRequest TupleBS8 BSL.ByteString
listAllPayments = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "payments"
    params = Params Nothing []

-- | Create a payment (fiat or Crypto)
-- https://developers.circle.com/developer/reference/payments-payments-create
createPayment :: CreatePaymentBody -> CircleAPIRequest PaymentRequest TupleBS8 BSL.ByteString
createPayment createPaymentBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "payments"
    params = Params (Just $ Body (encode createPaymentBody)) []

-- | Get a payment (fiat or Crypto)
-- https://developers.circle.com/developer/reference/payments-payments-get-id
getPayment :: UUID -> CircleAPIRequest PaymentRequest TupleBS8 BSL.ByteString
getPayment paymentId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "payments/" <> unUUID paymentId
    params = Params Nothing []

-- | Cancel a fiat payment
-- https://developers.circle.com/developer/reference/payments-payments-cancel-id
cancelPayment :: UUID -> CancelPaymentBody -> CircleAPIRequest PaymentRequest TupleBS8 BSL.ByteString
cancelPayment paymentId cancelPaymentBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "payments/" <> unUUID paymentId <> "/cancel"
    params = Params (Just $ Body (encode cancelPaymentBody)) []

-- | Refund a fiat payment
-- https://developers.circle.com/developer/reference/payments-payments-refund-id
refundPayment :: UUID -> RefundPaymentBody -> CircleAPIRequest PaymentRequest TupleBS8 BSL.ByteString
refundPayment paymentId refundPaymentBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "payments/" <> unUUID paymentId <> "/refund"
    params = Params (Just $ Body (encode refundPaymentBody)) []

-- | Create mock Silvergate payment SANDBOX ONLY
-- TODO constrain this method to be sandbox only.  Would be cool to do the same thing with the Production only methods
-- In the sandbox environment, initiate a mock SEN transfer that mimics the behavior of funds sent through the Silvergate SEN account linked to master wallet.
-- https://developers.circle.com/developer/reference/createmocksenpayment
createMockSilvergatePayment :: MockSenOrWirePaymentBodyParams -> CircleAPIRequest MockPaymentRequest TupleBS8 BSL.ByteString
createMockSilvergatePayment senBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "mocks/payments/sen"
    params = Params (Just $ Body (encode senBody)) []

-- | Create mock wire payment SANDBOX ONLY
-- TODO constrain this method to be sandbox only.  Would be cool to do the same thing with the Production only methods
-- In the sandbox environment, initiate a mock wire transfer that mimics the behavior of funds sent through the Silvergate SEN account linked to master wallet.
-- https://developers.circle.com/developer/reference/createmockwirepayment
createMockWirePayment :: MockSenOrWirePaymentBodyParams -> CircleAPIRequest MockPaymentRequest TupleBS8 BSL.ByteString
createMockWirePayment wireBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "mocks/payments/wire"
    params = Params (Just $ Body (encode wireBody)) []

-- | Create mock wire payment SANDBOX ONLY
-- TODO constrain this method to be sandbox only.  Would be cool to do the same thing with the Production only methods
-- In the sandbox environment, initiate a mock wire transfer that mimics the behavior of funds sent through the Silvergate SEN account linked to master wallet.
-- https://developers.circle.com/developer/reference/createmockwirepayment
createMockSEPAPayment :: MockSEPAPaymentBodyParams -> CircleAPIRequest MockPaymentRequest TupleBS8 BSL.ByteString
createMockSEPAPayment sepaBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "mocks/payments/sepa"
    params = Params (Just $ Body (encode sepaBody)) []

-- TODO add capture payment??

---------------------------------------------------------------
-- Utility methods for calling Circle's API
---------------------------------------------------------------
circle' ::
  CircleConfig ->
  CircleAPIRequest a TupleBS8 BSL.ByteString ->
  IO Reply
circle' CircleConfig {..} CircleAPIRequest {..} = do
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest $ T.unpack $ T.append (hostUri host) endpoint
  let reqBody
        | rMethod == NHTM.methodGet = mempty
        | isNothing (paramsBody params) = mempty
        | otherwise = unBody $ fromJust $ paramsBody params
      req =
        initReq
          { method = rMethod,
            requestBody = RequestBodyLBS reqBody,
            requestHeaders = [(hContentType, "application/json"), (hAccept, "application/json")],
            queryString = paramsToByteString $ paramsQuery params
          }
      circleToken = unApiToken token
      authorizedRequest = applyBearerAuth circleToken req
  httpLbs authorizedRequest manager

-- | Create a request to `circle`'s API
circle ::
  (FromJSON (CircleRequest a)) =>
  CircleConfig ->
  CircleAPIRequest a TupleBS8 BSL.ByteString ->
  IO (Either CircleError (CircleRequest a))
circle config request = do
  -- liftIO $ print request
  response <- circle' config request
  -- liftIO $ print response
  let result = eitherDecode $ responseBody response
  case result of
    Left s -> return (Left (CircleError s response))
    Right r -> return (Right r)

-- | This function is only used internally to speed up the test suite.
-- Instead of creating a new Manager we reuse the same one.
circleTest ::
  (FromJSON (CircleRequest a)) =>
  CircleConfig ->
  Manager ->
  CircleAPIRequest a TupleBS8 BSL.ByteString ->
  IO (Either CircleError (CircleRequest a))
circleTest config tlsManager request = do
  -- liftIO $ print request
  response <- circleTest' config request tlsManager
  -- liftIO $ print response
  let result = eitherDecode $ responseBody response
  case result of
    Left s -> return (Left (CircleError s response))
    Right r -> return (Right r)

circleTest' ::
  CircleConfig ->
  CircleAPIRequest a TupleBS8 BSL.ByteString ->
  Manager ->
  IO Reply
circleTest' CircleConfig {..} CircleAPIRequest {..} manager = do
  initReq <- parseRequest $ T.unpack $ T.append (hostUri host) endpoint
  let reqBody
        | rMethod == NHTM.methodGet = mempty
        | isNothing (paramsBody params) = mempty
        | otherwise = unBody $ fromJust $ paramsBody params
      req =
        initReq
          { method = rMethod,
            requestBody = RequestBodyLBS reqBody,
            requestHeaders = [(hContentType, "application/json"), (hAccept, "application/json")],
            queryString = paramsToByteString $ paramsQuery params
          }
      circleToken = unApiToken token
      authorizedRequest = applyBearerAuth circleToken req
  -- liftIO $ print (requestHeaders authorizedRequest)
  httpLbs authorizedRequest manager

-- | Conversion of a key value pair to a query parameterized string
paramsToByteString ::
  [Query] ->
  BS8.ByteString
paramsToByteString [] = mempty
paramsToByteString [x] = fst (unQuery x) <> "=" <> snd (unQuery x)
paramsToByteString (x : xs) =
  mconcat [fst $ unQuery x, "=", snd $ unQuery x, "&"] <> paramsToByteString xs