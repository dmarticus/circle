{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Unknot.Client where

import Unknot.Types
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Types ( FromJSON )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (isNothing, fromJust)
import qualified Data.Text as T
import Network.HTTP.Client
    ( httpLbs,
      newManager,
      applyBearerAuth,
      parseRequest,
      Manager,
      Request(method, requestBody, queryString),
      RequestBody(RequestBodyLBS),
      Response(responseBody) )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import qualified Network.HTTP.Types.Method  as NHTM

-- | Conversion of a key value pair to a query parameterized string
paramsToByteString ::
    [Query]
    -> BS8.ByteString
paramsToByteString []           = mempty
paramsToByteString [x] = fst (unQuery x) <> "=" <> snd (unQuery x)
paramsToByteString (x : xs) =
    mconcat [fst $ unQuery x, "=", snd $ unQuery x, "&"] <> paramsToByteString xs

-- | Create a bank account for a wire
-- https://developers.circle.com/reference/createbusinesswireaccount
createWireAccount :: WireAccountDetails -> CircleRequest WireAccountRequest TupleBS8 BSL.ByteString
createWireAccount wireAccountDetails = do
  mkCircleRequest NHTM.methodPost url params
  where
    url = "businessAccount/banks/wires"
    params = Params (Just $ Body (encode wireAccountDetails)) []

-- | Get a list of wire accounts
-- https://developers.circle.com/reference/listbusinesswireaccounts
getWireAccounts :: CircleRequest WireAccountsRequest TupleBS8 BSL.ByteString
getWireAccounts = do
  mkCircleRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/wires"
    params = Params Nothing []

-- | Get a single wire account, accepts the wire account Id as a parameter
-- https://developers.circle.com/reference/getbusinesswireaccount
getWireAccount :: T.Text -> CircleRequest WireAccountRequest TupleBS8 BSL.ByteString
getWireAccount wireAccountId = do
  mkCircleRequest NHTM.methodGet url params
  where
    url = T.append "businessAccount/banks/wires/" wireAccountId
    params = Params Nothing []

-- | Get the wire transfer instructions into the Circle bank account given your bank account id.
-- https://developers.circle.com/reference/getbusinesswireaccountinstructions
getWireAccountInstructions :: T.Text -> CircleRequest WireInstructionsRequest TupleBS8 BSL.ByteString
getWireAccountInstructions wireAccountId = do
  mkCircleRequest NHTM.methodGet url params
  where
    url = T.append "businessAccount/banks/wires/" wireAccountId <> "/instructions"
    params = Params Nothing []

-- | General methods
circle' :: CircleConfig
          -> CircleRequest a TupleBS8 BSL.ByteString
          -> IO (Response BSL.ByteString)
circle' CircleConfig {..} CircleRequest {..} = do
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest $ T.unpack $ T.append (hostUri host) endpoint
  let reqBody | rMethod == NHTM.methodGet = mempty
              | isNothing (paramsBody params) = mempty
              | otherwise = unBody $ fromJust $ paramsBody params
      req = initReq { method = rMethod
                    , requestBody = RequestBodyLBS reqBody
                    , queryString = paramsToByteString $ paramsQuery params
                    }
      circleToken = unApiToken token
      authorizedRequest = applyBearerAuth circleToken req
  httpLbs authorizedRequest manager

data CircleError =
  CircleError {
    parseError       :: String
  , circleResponse :: Response BSL.ByteString
  } deriving (Show)

-- | Create a request to `circle`'s API
circle
  :: (FromJSON (CircleReturn a))
  => CircleConfig
  -> CircleRequest a TupleBS8 BSL.ByteString
  -> IO (Either CircleError (CircleReturn a))
circle config request = do
  liftIO $ print request
  response <- circle' config request
  liftIO $ print response
  let result = eitherDecode $ responseBody response
  case result of
    Left s -> return (Left (CircleError s response))
    Right r -> return (Right r)

-- | This function is only used internally to speed up the test suite.
-- Instead of creating a new Manager we reuse the same one.
circleTest ::
     (FromJSON (CircleReturn a))
  => CircleConfig
  -> Manager
  -> CircleRequest a TupleBS8 BSL.ByteString
  -> IO (Either CircleError (CircleReturn a))
circleTest config tlsManager request = do
  liftIO $ print request
  response <- circleTest' config request tlsManager
  liftIO $ print response
  let result = eitherDecode $ responseBody response
  case result of
    Left s -> return (Left (CircleError s response))
    Right r -> return (Right r)

circleTest' :: CircleConfig
              -> CircleRequest a TupleBS8 BSL.ByteString
              -> Manager
              -> IO (Response BSL.ByteString)
circleTest' CircleConfig {..} CircleRequest {..} manager = do
  initReq <- parseRequest $ T.unpack $ T.append (hostUri host) endpoint
  let reqBody | rMethod == NHTM.methodGet = mempty
              | isNothing (paramsBody params) = mempty
              | otherwise = unBody $ fromJust $ paramsBody params
      req = initReq { method = rMethod
                    , requestBody = RequestBodyLBS reqBody
                    , queryString = paramsToByteString $ paramsQuery params
                    }
      circleToken = unApiToken token
      authorizedRequest = applyBearerAuth circleToken req
  httpLbs authorizedRequest manager
