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
import Network.HTTP.Types.Header (hContentType)
import qualified Network.HTTP.Types.Method as NHTM
import Unknot.Types

---------------------------------------------------------------
-- Wire endpoints
---------------------------------------------------------------
-- | Create a bank account for a wire
-- https://developers.circle.com/reference/createbusinesswireaccount
createWireAccount :: WireAccountDetails -> CircleAPIRequest WireAccountRequest TupleBS8 BSL.ByteString
createWireAccount wireAccountDetails = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/banks/wires"
    params = Params (Just $ Body (encode wireAccountDetails)) []

-- | Get a list of wire accounts
-- https://developers.circle.com/reference/listbusinesswireaccounts
getWireAccounts :: CircleAPIRequest WireAccountsRequest TupleBS8 BSL.ByteString
getWireAccounts = do
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
    url = "businessAccount/banks/wires/" <> (unUUID wireAccountId)
    params = Params Nothing []

-- | Get the wire transfer instructions into the Circle bank account given your bank account id.
-- https://developers.circle.com/reference/getbusinesswireaccountinstructions
-- TODO this needs to support a currency query param
getWireAccountInstructions :: UUID -> CircleAPIRequest WireInstructionsRequest TupleBS8 BSL.ByteString
getWireAccountInstructions wireAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/wires/" <> (unUUID wireAccountId) <> "/instructions"
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
createSubscription :: SubscriptionBody -> CircleAPIRequest SubscriptionsRequest TupleBS8 BSL.ByteString
createSubscription subscriptionBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "notifications/subscriptions"
    params = Params (Just $ Body (encode subscriptionBody)) []

-- | Delete subscription
-- https://developers.circle.com/reference/deletesubscribtion
deleteSubscription :: UUID -> CircleAPIRequest SubscriptionsRequest TupleBS8 BSL.ByteString
deleteSubscription resourceId = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "notifications/subscriptions" <> unUUID resourceId
    params = Params Nothing []

---------------------------------------------------------------
-- Payout endpoints
---------------------------------------------------------------
-- https://developers.circle.com/reference/listbusinessbalances
listAllPayouts :: CircleAPIRequest PayoutsRequest TupleBS8 BSL.ByteString
listAllPayouts = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/payouts"
    params = Params Nothing []

getPayout :: T.Text -> CircleAPIRequest PayoutRequest TupleBS8 BSL.ByteString
getPayout payoutId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/payouts" <> payoutId
    params = Params Nothing []

createPayout :: PayoutDetails -> CircleAPIRequest PayoutRequest TupleBS8 BSL.ByteString
createPayout payoutDetails = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/payouts"
    params = Params (Just $ Body (encode payoutDetails)) []

---------------------------------------------------------------
-- Utility methods for calling Circle's API
---------------------------------------------------------------
circle' ::
  CircleConfig ->
  CircleAPIRequest a TupleBS8 BSL.ByteString ->
  IO (Response BSL.ByteString)
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
            requestHeaders = [(hContentType, "application/json")],
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
  IO (Response BSL.ByteString)
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
            requestHeaders = [(hContentType, "application/json")],
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