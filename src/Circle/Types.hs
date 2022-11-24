-------------------------------------------
-------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Circle.Types
-- Copyright   : (c) Dylan Martin, 2022
-- Maintainer  : dmarticus@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\developers.circle.com/developer/v1/docs/circle-api-resources >
module Circle.Types where

import Autodocodec
  ( Autodocodec (Autodocodec),
    HasCodec (codec),
    bimapCodec,
    dimapCodec,
    object,
    optionalField',
    requiredField,
    requiredField',
    shownBoundedEnumCodec,
    stringConstCodec,
    (.=),
  )
import Control.Monad (guard)
import Country
  ( Country,
    alphaTwoUpper,
  )
import Data.Aeson
  ( FromJSON (parseJSON),
    Result (Error, Success),
    ToJSON (toEncoding, toJSON),
    withObject,
    withText,
    (.:),
    (.:?),
  )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (fromJSON)
import Data.Bifunctor
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Coerce (coerce)
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.UUID
import Data.UUID qualified as UUID
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift)
import Network.HTTP.Client (Response)
import Network.HTTP.Types.Method qualified as NHTM
import Refined
import Refined.Unsafe (reallyUnsafeRefine)
import System.Environment (getEnv)
import Text.Regex.PCRE.Heavy

---------------------------------------------------------------
-- Circle API wrapper
---------------------------------------------------------------

-- | Token type used for HTTP Bearer authentication.
newtype ApiToken = ApiToken
  { unApiToken :: BS8.ByteString
  }
  deriving (Read, Show, Eq)

-- | Type to represent the 3 main components (method, endpoint, and params)
-- needed to call Circle's API.
data CircleAPIRequest a b c = CircleAPIRequest
  { -- | Method of CircleAPIRequest
    rMethod :: !Method,
    -- | Endpoint of CircleAPIRequest
    endpoint :: !Text,
    -- | Request params of CircleAPIRequest
    params :: !(Params TupleBS8 BSL.ByteString)
  }
  deriving (Show)

-- | Create a @CircleAPIRequest@
mkCircleAPIRequest ::
  Method ->
  Text ->
  Params TupleBS8 BSL.ByteString ->
  CircleAPIRequest a b c
mkCircleAPIRequest = CircleAPIRequest

type family CircleRequest a :: *

-- | CircleErrors have contain both the error reason (`parseError`) and the
-- full error response body as a ByteString.
data CircleError = CircleError
  { parseError :: !Text,
    errorResponseBody :: !Reply
  }
  deriving (Show)

-- | The CircleResponseBody will have `Nothing` for the `circleResponseCode` and
-- `circleResponseMessage` if the request succeeds, and `Nothing` for the `circleResponseData`
-- if the request fails.
data CircleResponseBody a = CircleResponseBody
  { circleResponseCode :: !(Maybe ResponseStatus),
    circleResponseMessage :: !(Maybe ResponseMessage),
    circleResponseData :: !(Maybe a)
  }
  deriving (Eq, Show)

-- NB: This FromJSON instance parses every response from the Circle API, so I can't use Autodocodec to derive FromJSON here
-- because I'm using ThisOrThat (see below) as a smart parser for certain types, and that type doesn't have Autodocodec instances
instance FromJSON a => FromJSON (CircleResponseBody a) where
  parseJSON = withObject "CircleResponseBody" parse
    where
      parse o =
        CircleResponseBody
          <$> o .:? "status"
          <*> o .:? "message"
          <*> o .:? "data"

-- Utility types have for interacting with the data returned from Circle's actual API
newtype ResponseStatus = ResponseStatus
  { unResponseStatus :: Int
  }
  deriving (Eq, Show, FromJSON)

instance HasCodec ResponseStatus where
  codec = dimapCodec ResponseStatus unResponseStatus codec

newtype ResponseMessage = ResponseMessage
  { unResponseMessage :: Text
  }
  deriving (Eq, Show, FromJSON)

instance HasCodec ResponseMessage where
  codec = dimapCodec ResponseMessage unResponseMessage codec

type Reply = Response BSL.ByteString

type Method = NHTM.Method

-- | Either production or sandbox API host
type Host = Text

data CircleHost
  = CircleProduction
  | CircleSandbox
  deriving (Eq, Show)

hostUri :: CircleHost -> Text
hostUri CircleProduction = "https://api.circle.com/v1/"
hostUri CircleSandbox = "https://api-sandbox.circle.com/v1/"

data CircleConfig = CircleConfig
  { host :: !CircleHost,
    token :: !ApiToken
  }
  deriving (Eq, Show)

-- | Creates an API token using a secrete stored at `CIRCLE_API_KEY` (the default key for storing the Circle secret)
credentialsEnv :: Maybe String -> IO ApiToken
credentialsEnv mKey = do
  key <- case mKey of
    Just k -> pure k
    Nothing -> pure "CIRCLE_API_KEY"
  token <- getEnv key
  return (ApiToken $ BS8.pack token)

-- | Helper method for instantiating a Circle config that calls the production endpoint: https://api.circle.com/v1/
-- Example usage:
-- @
-- import Circle.Client
-- import Circle.Types
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   config <- prodEnvConfig "CIRCLE_API_KEY"
--   result <- circle config manager getConfigurationInfo
--   case result of
--     Right CircleResponseBody b -> print bs
--     Left CircleError e -> print e
-- @
prodEnvConfig :: Maybe String -> IO CircleConfig
prodEnvConfig key = do
  CircleConfig CircleProduction <$> credentialsEnv key

-- | Helper method for instantiating a Circle config that calls the production endpoint: https://api-sandbox.circle.com/v1/
-- Example usage:
-- @
-- import Circle.Client
-- import Circle.Types
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   config <- sandboxEnvConfig "CIRCLE_API_KEY"
--   result <- circle config manager getConfigurationInfo
--   case result of
--     Right CircleResponseBody b -> print bs
--     Left CircleError e -> print e
-- @
sandboxEnvConfig :: Maybe String -> IO CircleConfig
sandboxEnvConfig key = do
  CircleConfig CircleSandbox <$> credentialsEnv key

newtype Query = Query
  { unQuery :: TupleBS8
  }
  deriving (Eq, Show)

newtype Body = Body
  { unBody :: BSL.ByteString
  }
  deriving (Eq, Show)

-- | Parameters for each request which include both the query and the body of a request
data Params b c = Params
  { paramsBody :: Maybe Body,
    paramsQuery :: ![Query]
  }
  deriving (Show)

joinQueryParams :: Params b c -> Params b c -> Params b c
joinQueryParams (Params _ xs) (Params b ys) = Params b (xs ++ ys)

-- | Type alias for query parameters
type TupleBS8 = (BS8.ByteString, BS8.ByteString)

-- | Convert a parameter to a key/value
class ToCircleParam param where
  toCircleParam :: param -> Params TupleBS8 c -> Params TupleBS8 c

class (ToCircleParam param) => CircleHasParam request param

-- | Supports adding an optional query parameter.
-- Example usage:
-- @
-- import Circle.Client
-- import Circle.Types
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   config <- sandboxEnvConfig "CIRCLE_API_KEY"
--   result <- circle config manager listAllBalances -&- PaginationQueryParams (PageBefore "a8899b8e-782a-4526-b674-0efe1e04526d")
--   case result of
--     Right CircleResponseBody b -> print bs
--     Left CircleError e -> print e
-- @
(-&-) ::
  CircleHasParam r param =>
  CircleAPIRequest r b c ->
  param ->
  CircleAPIRequest r b c
circleAPIRequest -&- param =
  circleAPIRequest
    { params = toCircleParam param (params circleAPIRequest)
    }

---------------------------------------------------------------
-- Query parameters
---------------------------------------------------------------

newtype PaginationQueryParams = PaginationQueryParams
  { paginationQueryParams :: PaginationQueryParam
  }
  deriving (Eq, Show)

-- | Depending on which endpoint is being called, the IDs after the `PageBefore` and `PageAfter` params could either be UUIDs or non-UUIDs. Let's just keep them as text for now.
-- TODO maybe improve this one day.
data PaginationQueryParam = PageBefore !Text | PageAfter !Text deriving (Show, Eq)

-- | Circle has some BS pagination where they let users supply some canonical
-- collection ID, and then this pagination rule will return `n` entries before OR after that page,
-- where `n` is controlled by the pageSize param.  This type exists to prevent callers from providing both params, which would error out
instance ToCircleParam PaginationQueryParams where
  toCircleParam (PaginationQueryParams p) =
    case p of
      PageBefore a ->
        joinQueryParams $ Params Nothing [Query ("pageBefore", TE.encodeUtf8 a)]
      PageAfter a ->
        joinQueryParams $ Params Nothing [Query ("pageAfter", TE.encodeUtf8 a)]

newtype FromQueryParam = FromQueryParam
  { fromQueryParam :: UTCTime
  }
  deriving (Eq, Show)

instance ToCircleParam FromQueryParam where
  toCircleParam (FromQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("from", TE.encodeUtf8 $ utcToCircle i)]

newtype ToQueryParam = ToQueryParam
  { toQueryParam :: UTCTime
  }
  deriving (Eq, Show)

instance ToCircleParam ToQueryParam where
  toCircleParam (ToQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("to", TE.encodeUtf8 $ utcToCircle i)]

newtype PageSizeQueryParam = PageSizeQueryParam
  { pageSizeQueryParam :: Integer
  }
  deriving (Eq, Show)

instance ToCircleParam PageSizeQueryParam where
  toCircleParam (PageSizeQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("pageSize", TE.encodeUtf8 $ (T.pack . show) i)]

newtype StatusQueryParams = StatusQueryParams
  { statusQueryParams :: [Status]
  }
  deriving (Eq, Show)

statusToBS8 :: Status -> BS8.ByteString
statusToBS8 Pending = "pending"
statusToBS8 Complete = "complete"
statusToBS8 Failed = "failed"

instance ToCircleParam StatusQueryParams where
  toCircleParam (StatusQueryParams xs) =
    joinQueryParams $ Params Nothing [Query ("status", BS8.intercalate "," (map statusToBS8 xs))]

newtype DestinationQueryParam = DestinationQueryParam
  { destinationQueryParam :: UUID
  }
  deriving (Eq, Show)

instance ToCircleParam DestinationQueryParam where
  toCircleParam (DestinationQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("destination", TE.encodeUtf8 (tshow i))]

newtype TypeQueryParam = TypeQueryParam
  { typeQueryParam :: BankAccountType
  }
  deriving (Eq, Show)

bankAccountTypeToBS8 :: BankAccountType -> BS8.ByteString
bankAccountTypeToBS8 Wire = "wire"
bankAccountTypeToBS8 Sen = "sen"

instance ToCircleParam TypeQueryParam where
  toCircleParam (TypeQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("type", bankAccountTypeToBS8 i)]

newtype CurrencyQueryParam = CurrencyQueryParam
  { currencyQueryParam :: SupportedCurrencies
  }
  deriving (Eq, Show)

currencyToBS8 :: SupportedCurrencies -> BS8.ByteString
currencyToBS8 USD = "USD"
currencyToBS8 EUR = "EUR"
currencyToBS8 BTC = "BTC"
currencyToBS8 ETH = "ETH"

instance ToCircleParam CurrencyQueryParam where
  toCircleParam (CurrencyQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("currency", currencyToBS8 i)]

newtype SourceQueryParam = SourceQueryParam
  { sourceQueryParam :: UUID
  }
  deriving (Eq, Show)

instance ToCircleParam SourceQueryParam where
  toCircleParam (SourceQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("source", TE.encodeUtf8 (tshow i))]

newtype SettlementIdQueryParam = SettlementIdQueryParam
  { settlementIdQueryParam :: UUID
  }
  deriving (Eq, Show)

instance ToCircleParam SettlementIdQueryParam where
  toCircleParam (SettlementIdQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("settlementId", TE.encodeUtf8 (tshow i))]

newtype PaymentIntentIdQueryParam = PaymentIntentIdQueryParam
  { paymentIntentIdQueryParam :: UUID
  }
  deriving (Eq, Show)

instance ToCircleParam PaymentIntentIdQueryParam where
  toCircleParam (PaymentIntentIdQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("paymentIntentId", TE.encodeUtf8 (tshow i))]

newtype PaymentIdQueryParam = PaymentIdQueryParam
  { paymentIdQueryParam :: UUID
  }
  deriving (Eq, Show)

instance ToCircleParam PaymentIdQueryParam where
  toCircleParam (PaymentIdQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("paymentId", TE.encodeUtf8 (tshow i))]

newtype PaymentStatusQueryParams = PaymentStatusQueryParams
  { paymentStatusQueryParams :: [PaymentStatus]
  }
  deriving (Eq, Show)

paymentStatusToBS8 :: PaymentStatus -> BS8.ByteString
paymentStatusToBS8 PaymentPending = "pending"
paymentStatusToBS8 Confirmed = "confirmed"
paymentStatusToBS8 PaymentFailed = "failed"
paymentStatusToBS8 Paid = "paid"
paymentStatusToBS8 ActionRequired = "action_required"

instance ToCircleParam PaymentStatusQueryParams where
  toCircleParam (PaymentStatusQueryParams xs) =
    joinQueryParams $ Params Nothing [Query ("status", BS8.intercalate "," (map paymentStatusToBS8 xs))]

newtype PaymentIntentContextQueryParams = PaymentIntentContextQueryParams
  { paymentIntentContextQueryParams :: [PaymentIntentContext]
  }
  deriving (Eq, Show)

paymentIntentContextToBS8 :: PaymentIntentContext -> BS8.ByteString
paymentIntentContextToBS8 ContextUnderpaid = "underpaid"
paymentIntentContextToBS8 ContextPaid = "paid"
paymentIntentContextToBS8 ContextOverpaid = "overpaid"

instance ToCircleParam PaymentIntentContextQueryParams where
  toCircleParam (PaymentIntentContextQueryParams xs) =
    joinQueryParams $ Params Nothing [Query ("context", BS8.intercalate "," (map paymentIntentContextToBS8 xs))]

newtype WalletIdQueryParam = WalletIdQueryParam
  { walletIdQueryParam :: WalletId
  }
  deriving (Eq, Show)

instance ToCircleParam WalletIdQueryParam where
  toCircleParam (WalletIdQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("walletId", TE.encodeUtf8 (unWalletId i))]

newtype SourceWalletIdQueryParam = SourceWalletIdQueryParam
  { sourceWalletIdQueryParam :: WalletId
  }
  deriving (Eq, Show)

instance ToCircleParam SourceWalletIdQueryParam where
  toCircleParam (SourceWalletIdQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("sourceWalletId", TE.encodeUtf8 (unWalletId i))]

newtype DestinationWalletIdQueryParam = DestinationWalletIdQueryParam
  { destinationWalletIdQueryParam :: WalletId
  }
  deriving (Eq, Show)

instance ToCircleParam DestinationWalletIdQueryParam where
  toCircleParam (DestinationWalletIdQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("destinationWalletId", TE.encodeUtf8 (unWalletId i))]

newtype ReturnIdentitiesQueryParam = ReturnIdentitiesQueryParam
  { returnIdentitiesQueryParam :: Bool
  }
  deriving (Eq, Show)

instance ToCircleParam ReturnIdentitiesQueryParam where
  toCircleParam (ReturnIdentitiesQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("returnIdentities", TE.encodeUtf8 (T.pack (show i)))]

---------------------------------------------------------------
-- Balance Types
---------------------------------------------------------------

data BalanceRequest

type instance CircleRequest BalanceRequest = CircleResponseBody BalanceResponseBody

data BalanceResponseBody = BalanceResponseBody
  { balanceResponseBodyAvailable :: ![MoneyAmount],
    balanceResponseBodyUnsettled :: ![MoneyAmount]
  }
  deriving (Eq, Show, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec BalanceResponseBody)

instance HasCodec BalanceResponseBody where
  codec =
    object "BalanceResponseBody" $
      BalanceResponseBody
        <$> requiredField' "available" .= balanceResponseBodyAvailable
        <*> requiredField' "unsettled" .= balanceResponseBodyAvailable

---------------------------------------------------------------
-- Payout Types
---------------------------------------------------------------
data PayoutRequest

type instance CircleRequest PayoutRequest = CircleResponseBody PayoutResponseBody

data PayoutsRequest

type instance CircleRequest PayoutsRequest = CircleResponseBody [PayoutResponseBody]

instance CircleHasParam PayoutsRequest PaginationQueryParams

instance CircleHasParam PayoutsRequest FromQueryParam

instance CircleHasParam PayoutsRequest ToQueryParam

instance CircleHasParam PayoutsRequest PageSizeQueryParam

instance CircleHasParam PayoutsRequest StatusQueryParams

instance CircleHasParam PayoutsRequest TypeQueryParam

instance CircleHasParam PayoutsRequest DestinationQueryParam

data PayoutResponseBody = PayoutResponseBody
  { payoutResponseBodyId :: !UUID,
    payoutResponseBodySourceWalletId :: !WalletId,
    payoutResponseBodyDestinationBankAccount :: !DestinationBankAccount,
    payoutResponseBodyAmount :: !MoneyAmount,
    payoutResponseBodyFees :: !MoneyAmount,
    payoutResponseBodyStatus :: !Status,
    payoutResponseBodyTrackingRef :: !TrackingReference,
    payoutResponseBodyPayoutErrorCode :: !PayoutErrorCode,
    payoutResponseBodyRiskEvaluation :: !RiskEvaluation,
    payoutResponseBodyAdjustments :: !Adjustments,
    payoutResponseBodyPayoutReturn :: !PayoutReturnResponseBody,
    payoutResponseBodyCreateDate :: !UTCTime,
    payoutResponseBodyUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec PayoutResponseBody)

instance HasCodec PayoutResponseBody where
  codec =
    object "PayoutResponseBody" $
      PayoutResponseBody
        <$> requiredField' "id" .= payoutResponseBodyId
        <*> requiredField' "sourceWalletId" .= payoutResponseBodySourceWalletId
        <*> requiredField' "destination" .= payoutResponseBodyDestinationBankAccount
        <*> requiredField' "amount" .= payoutResponseBodyAmount
        <*> requiredField' "fees" .= payoutResponseBodyFees
        <*> requiredField' "status" .= payoutResponseBodyStatus
        <*> requiredField' "trackingRef" .= payoutResponseBodyTrackingRef
        <*> requiredField' "errorCode" .= payoutResponseBodyPayoutErrorCode
        <*> requiredField' "riskEvaluation" .= payoutResponseBodyRiskEvaluation
        <*> requiredField' "adjustments" .= payoutResponseBodyAdjustments
        <*> requiredField' "payoutReturn" .= payoutResponseBodyPayoutReturn
        <*> requiredField' "createDate" .= payoutResponseBodyCreateDate
        <*> requiredField' "updateDate" .= payoutResponseBodyUpdateDate

-- | Request body for creating a new business account payout
data BusinessPayoutRequestBody = BusinessPayoutRequestBody
  { businessPayoutIdempotencyKey :: !UUID,
    businessPayoutDestination :: !DestinationBankAccount,
    businessPayoutAmount :: !MoneyAmount
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec BusinessPayoutRequestBody)

instance HasCodec BusinessPayoutRequestBody where
  codec =
    object "BusinessPayoutRequestBody" $
      BusinessPayoutRequestBody
        <$> requiredField' "idempotencyKey" .= businessPayoutIdempotencyKey
        <*> requiredField' "destination" .= businessPayoutDestination
        <*> requiredField' "amount" .= businessPayoutAmount

newtype PayoutMetadata = PayoutMetadata {payoutMetadataBeneficiaryEmail :: Email}
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON) via (Autodocodec PayoutMetadata)

instance HasCodec PayoutMetadata where
  codec =
    object "PayoutMetadata" $
      PayoutMetadata
        <$> requiredField' "beneficiaryEmail" .= payoutMetadataBeneficiaryEmail

-- | Request body to create a payout.
data PayoutRequestBody = PayoutRequestBody
  { payoutIdempotencyKey :: !UUID,
    payoutSource :: !(Maybe PaymentSource),
    payoutDestination :: !DestinationBankAccount,
    payoutAmount :: !MoneyAmount,
    payoutMetadata :: !PayoutMetadata
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec PayoutRequestBody)

instance HasCodec PayoutRequestBody where
  codec =
    object "PayoutRequestBody" $
      PayoutRequestBody
        <$> requiredField' "idempotencyKey" .= payoutIdempotencyKey
        <*> optionalField' "source" .= payoutSource
        <*> requiredField' "destination" .= payoutDestination
        <*> requiredField' "amount" .= payoutAmount
        <*> requiredField' "metadata" .= payoutMetadata

data PayoutReturnResponseBody = PayoutReturnResponseBody
  { payoutReturnResponseBodyId :: !UUID,
    payoutReturnResponseBodyOriginalPayoutId :: !UUID,
    payoutReturnResponseBodyAmount :: !MoneyAmount,
    payoutReturnResponseBodyFees :: !MoneyAmount,
    payoutReturnResponseBodyReason :: !Text,
    payoutReturnResponseBodyStatus :: !Status,
    payoutReturnResponseBodyCreateDate :: !UTCTime,
    payoutReturnResponseBodyUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec PayoutReturnResponseBody)

instance HasCodec PayoutReturnResponseBody where
  codec =
    object "PayoutReturnResponseBody" $
      PayoutReturnResponseBody
        <$> requiredField' "id" .= payoutReturnResponseBodyId
        <*> requiredField' "payoutId" .= payoutReturnResponseBodyOriginalPayoutId
        <*> requiredField' "amount" .= payoutReturnResponseBodyAmount
        <*> requiredField' "fees" .= payoutReturnResponseBodyFees
        <*> requiredField' "reason" .= payoutReturnResponseBodyReason
        <*> requiredField' "status" .= payoutReturnResponseBodyStatus
        <*> requiredField' "createDate" .= payoutReturnResponseBodyCreateDate
        <*> requiredField' "updateDate" .= payoutReturnResponseBodyUpdateDate

data PayoutErrorCode
  = InsufficientFunds
  | TransactionDenied
  | TransactionFailed
  | TransactionReturned
  | BankTransactionError
  | FiatAccountLimitExceeded
  | InvalidBankAccountNumber
  | InvalidACHRoutingTransitNumber
  | InvalidWireRoutingTransitNumber
  | VendorInactive
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PayoutErrorCode)

instance HasCodec PayoutErrorCode where
  codec =
    stringConstCodec $
      NE.fromList
        [ (InsufficientFunds, "insufficient_funds"),
          (TransactionDenied, "transaction_denied"),
          (TransactionFailed, "transaction_failed"),
          (TransactionReturned, "transaction_returned"),
          (BankTransactionError, "bank_transaction_error"),
          (FiatAccountLimitExceeded, "fiat_account_limit_exceeded"),
          (InvalidBankAccountNumber, "invalid_bank_account_number"),
          (InvalidACHRoutingTransitNumber, "invalid_ach_rtn"),
          (InvalidWireRoutingTransitNumber, "invalid_wire_rtn"),
          (VendorInactive, "vendor_inactive")
        ]

---------------------------------------------------------------
-- Configuration Types
---------------------------------------------------------------

data ConfigurationRequest

type instance CircleRequest ConfigurationRequest = CircleResponseBody ConfigurationResponseBody

newtype ConfigurationResponseBody = ConfigurationResponseBody {configurationResponseBodyPayments :: WalletConfig}
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ConfigurationResponseBody)

instance HasCodec ConfigurationResponseBody where
  codec =
    object "ConfigurationResponseBody" $
      ConfigurationResponseBody
        <$> requiredField' "payments" .= configurationResponseBodyPayments

newtype WalletConfig = WalletConfig {masterWalletId :: WalletId}
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec WalletConfig)

instance HasCodec WalletConfig where
  codec =
    object "WalletConfig" $
      WalletConfig
        <$> requiredField' "masterWalletId" .= masterWalletId

---------------------------------------------------------------
-- Encryption Types
---------------------------------------------------------------

data EncryptionRequest

type instance CircleRequest EncryptionRequest = CircleResponseBody EncryptionResponseBody

data EncryptionResponseBody = EncryptionResponseBody
  { encryptionResponseBodyKeyId :: !Text, -- TODO this should actually be a UUID, but for the tests to work it needs to be relaxed a bit
    encryptionResponseBodyPublicKey :: !PGPKey
  }
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON) via (Autodocodec EncryptionResponseBody)

newtype PGPKey = PGPKey
  { unPGPKey :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec PGPKey where
  codec = dimapCodec PGPKey unPGPKey codec

instance HasCodec EncryptionResponseBody where
  codec =
    object "EncryptionResponseBody" $
      EncryptionResponseBody
        <$> requiredField' "keyId" .= encryptionResponseBodyKeyId
        <*> requiredField' "publicKey" .= encryptionResponseBodyPublicKey

---------------------------------------------------------------
-- Channels Types
---------------------------------------------------------------

data ChannelsRequest

type instance CircleRequest ChannelsRequest = CircleResponseBody ChannelResponseBody

newtype ChannelResponseBody = ChannelResponseBody {channels :: [Channel]}
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ChannelResponseBody)

instance HasCodec ChannelResponseBody where
  codec =
    object "ChannelResponseBody" $
      ChannelResponseBody
        <$> requiredField' "channels" .= channels

data Channel = Channel
  { channelId :: !UUID,
    channelDefault :: !Bool,
    channelCardDescriptor :: !Text,
    channelAchDescriptor :: !Text
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec Channel)

instance HasCodec Channel where
  codec =
    object "Channel" $
      Channel
        <$> requiredField' "id" .= channelId
        <*> requiredField' "default" .= channelDefault
        <*> requiredField' "cardDescriptor" .= channelCardDescriptor
        <*> requiredField' "achDescriptor" .= channelAchDescriptor

---------------------------------------------------------------
-- Stablecoin Types
---------------------------------------------------------------

data StablecoinsRequest

type instance CircleRequest StablecoinsRequest = CircleResponseBody [StablecoinResponseBody]

data StablecoinResponseBody = StablecoinResponseBody
  { stablecoinResponseBodyName :: !Text,
    stablecoinResponseBodySymbol :: !Stablecoin,
    stablecoinResponseBodyTotalAmount :: !Text, --TODO need an amount field that supports crypto depth.  Many of these coins go past a fixed integer depth.
    stablecoinResponseBodyChains :: ![ChainAmount]
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec StablecoinResponseBody)

instance HasCodec StablecoinResponseBody where
  codec =
    object "StablecoinResponseBody" $
      StablecoinResponseBody
        <$> requiredField' "name" .= stablecoinResponseBodyName
        <*> requiredField' "symbol" .= stablecoinResponseBodySymbol
        <*> requiredField' "totalAmount" .= stablecoinResponseBodyTotalAmount
        <*> requiredField' "chains" .= stablecoinResponseBodyChains

data ChainAmount = ChainAmount
  { chainAmountAmount :: !Text,
    chainAmountChain :: !Chain,
    chainAmountUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ChainAmount)

instance HasCodec ChainAmount where
  codec =
    object "ChainAmount" $
      ChainAmount
        <$> requiredField' "amount" .= chainAmountAmount
        <*> requiredField' "chain" .= chainAmountChain
        <*> requiredField' "updateDate" .= chainAmountUpdateDate

data Chain = ALGO | ARB | AVAX | ChainBTC | ChainETH | FLOW | HBAR | MATIC | NEAR | OP | SOL | TRX | XLM
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec Chain)

instance HasCodec Chain where
  codec =
    stringConstCodec $
      NE.fromList
        [ (ALGO, "ALGO"),
          (ARB, "ARB"),
          (AVAX, "AVAX"),
          (ChainBTC, "BTC"),
          (ChainETH, "ETH"),
          (FLOW, "FLOW"),
          (HBAR, "HBAR"),
          (MATIC, "MATIC"),
          (NEAR, "NEAR"),
          (OP, "OP"),
          (SOL, "SOL"),
          (TRX, "TRX"),
          (XLM, "XLM")
        ]

data Stablecoin = USDC | EUROC | USDT
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec Stablecoin)

instance HasCodec Stablecoin where
  codec =
    stringConstCodec $
      NE.fromList
        [ (USDC, "USDC"),
          (EUROC, "EUROC"),
          (USDT, "USDT")
        ]

---------------------------------------------------------------
-- Subscription Types
---------------------------------------------------------------

data SubscriptionsRequest

type instance CircleRequest SubscriptionsRequest = CircleResponseBody [SubscriptionResponseBody]

data SubscriptionRequest

type instance CircleRequest SubscriptionRequest = CircleResponseBody SubscriptionResponseBody

data SubscriptionResponseBody = SubscriptionResponseBody
  { subscriptionResponseBodyId :: !UUID,
    subscriptionResponseBodyEndpoint :: !URL,
    subscriptionResponseBodySubscriptionDetails :: ![SubscriptionDetails]
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SubscriptionResponseBody)

instance HasCodec SubscriptionResponseBody where
  codec =
    object "SubscriptionResponseBody" $
      SubscriptionResponseBody
        <$> requiredField' "id" .= subscriptionResponseBodyId
        <*> requiredField' "endpoint" .= subscriptionResponseBodyEndpoint
        <*> requiredField' "subscriptionDetails" .= subscriptionResponseBodySubscriptionDetails

data SubscriptionDetails = SubscriptionDetails
  { subscriptionDetailsUrl :: !URL,
    subscriptionDetailsStatus :: !Status
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SubscriptionDetails)

instance HasCodec SubscriptionDetails where
  codec =
    object "SubscriptionDetails" $
      SubscriptionDetails
        <$> requiredField' "url" .= subscriptionDetailsUrl
        <*> requiredField' "status" .= subscriptionDetailsStatus

-- | Request body for creating a new subscription
newtype SubscriptionRequestBody = SubscriptionRequestBody
  { subscriptionRequestBodyEndpoint :: Text
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SubscriptionRequestBody)

instance HasCodec SubscriptionRequestBody where
  codec =
    object "SubscriptionRequestBody" $
      SubscriptionRequestBody
        <$> requiredField' "endpoint" .= subscriptionRequestBodyEndpoint

---------------------------------------------------------------
-- Transfer Types
---------------------------------------------------------------

data TransfersRequest

type instance CircleRequest TransfersRequest = CircleResponseBody [TransferResponseBody]

instance CircleHasParam TransfersRequest PaginationQueryParams

instance CircleHasParam TransfersRequest FromQueryParam

instance CircleHasParam TransfersRequest ToQueryParam

instance CircleHasParam TransfersRequest PageSizeQueryParam

instance CircleHasParam TransfersRequest WalletIdQueryParam

instance CircleHasParam TransfersRequest SourceWalletIdQueryParam

instance CircleHasParam TransfersRequest DestinationWalletIdQueryParam

instance CircleHasParam TransfersRequest ReturnIdentitiesQueryParam

data TransferRequest

type instance CircleRequest TransferRequest = CircleResponseBody TransferResponseBody

instance CircleHasParam TransferRequest ReturnIdentitiesQueryParam

-- | Request body for creating a new business account transfer
data BusinessTransferRequestBody = BusinessTransferRequestBody
  { businessTransferRequestBodyIdempotencyKey :: !UUID,
    businessTransferRequestBodyDestination :: !TransferDestination,
    businessTransferRequestBodyAmount :: !MoneyAmount
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec BusinessTransferRequestBody)

instance HasCodec BusinessTransferRequestBody where
  codec =
    object "BusinessTransferRequestBody" $
      BusinessTransferRequestBody
        <$> requiredField' "idempotencyKey" .= businessTransferRequestBodyIdempotencyKey
        <*> requiredField' "destination" .= businessTransferRequestBodyDestination
        <*> requiredField' "amount" .= businessTransferRequestBodyAmount

-- | Request body for creating a new transfer
data TransferRequestBody = TransferRequestBody
  { transferRequestBodyIdempotencyKey :: !UUID,
    transferRequestBodySource :: !PaymentSource,
    transferRequestBodyDestination :: !TransferDestination,
    transferRequestBodyAmount :: !MoneyAmount
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec TransferRequestBody)

instance HasCodec TransferRequestBody where
  codec =
    object "TransferRequestBody" $
      TransferRequestBody
        <$> requiredField' "idempotencyKey" .= transferRequestBodyIdempotencyKey
        <*> requiredField' "source" .= transferRequestBodySource
        <*> requiredField' "destination" .= transferRequestBodyDestination
        <*> requiredField' "amount" .= transferRequestBodyAmount

data TransferDestination = TransferDestination
  { transferDestinationType :: !DestinationType,
    transferDestinationAddressId :: !UUID
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec TransferDestination)

instance HasCodec TransferDestination where
  codec =
    object "TransferDestination" $
      TransferDestination
        <$> requiredField' "type" .= transferDestinationType
        <*> requiredField' "addressId" .= transferDestinationAddressId

data DestinationType = VerifiedBlockchain
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec DestinationType)

instance HasCodec DestinationType where
  codec =
    stringConstCodec $
      NE.fromList
        [ (VerifiedBlockchain, "verified_blockchain")
        ]

data TransferResponseBody = TransferResponseBody
  { transferResponseBodyId :: !UUID,
    transferResponseBodySource :: !(ThisOrThat SourceWallet SourceBlockchain),
    transferResponseBodyDestination :: !(ThisOrThat DestinationWallet DestinationBlockchain),
    transferResponseBodyAmount :: !MoneyAmount,
    transferResponseBodyFees :: !TransferFeeAmount,
    transferResponseBodyTransactionHash :: !(Maybe HexString),
    transferResponseBodyStatus :: !Status,
    transferResponseBodyTransferErrorCode :: !(Maybe TransferErrorCode),
    transferResponseBodyRiskEvaluation :: !(Maybe RiskEvaluation),
    transferResponseBodyCreateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

-- NB: this doesn't use autodocodec for deriving ToJSON and FromJSON since I'm using the hand-rolled
-- ThisOrThat helper for smartly parsing types.
instance FromJSON TransferResponseBody where
  parseJSON = withObject "TransferResponseBody" parse
    where
      parse o =
        TransferResponseBody
          <$> o .: "id"
          <*> o .: "source"
          <*> o .: "destination"
          <*> o .: "amount"
          <*> o .: "fees"
          <*> o .:? "transactionHash"
          <*> o .: "status"
          <*> o .:? "errorCode"
          <*> o .:? "riskEvaluation"
          <*> o .:? "createDate"

data SourceWallet = SourceWallet
  { sourceWalletType :: !TransferType,
    sourceWalletId :: !WalletId,
    sourceWalletIdentities :: ![Identity]
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SourceWallet)

instance HasCodec SourceWallet where
  codec =
    object "SourceWallet" $
      SourceWallet
        <$> requiredField' "type" .= sourceWalletType
        <*> requiredField' "id" .= sourceWalletId
        <*> requiredField' "identities" .= sourceWalletIdentities

data SourceBlockchain = SourceBlockchain
  { sourceBlockchainType :: !TransferType,
    sourceBlockchainChain :: !Chain,
    sourceBlockChainIdentities :: ![Identity]
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SourceBlockchain)

instance HasCodec SourceBlockchain where
  codec =
    object "SourceBlockchain" $
      SourceBlockchain
        <$> requiredField' "type" .= sourceBlockchainType
        <*> requiredField' "chain" .= sourceBlockchainChain
        <*> requiredField' "identities" .= sourceBlockChainIdentities

data DestinationWallet = DestinationWallet
  { destinationWalletType :: !TransferType,
    destinationWalletId :: !WalletId,
    destinationWalletAddress :: !(Maybe Text),
    destinationWalletAddressTag :: !(Maybe Text)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec DestinationWallet)

instance HasCodec DestinationWallet where
  codec =
    object "DestinationWallet" $
      DestinationWallet
        <$> requiredField' "type" .= destinationWalletType
        <*> requiredField' "id" .= destinationWalletId
        <*> optionalField' "address" .= destinationWalletAddress
        <*> optionalField' "addressTag" .= destinationWalletAddressTag

data DestinationBlockchain = DestinationBlockchain
  { destinationBlockchainType :: !TransferType,
    destinationBlockchainAddress :: !HexString,
    destinationBlockchainAddressTag :: !(Maybe Text),
    destinationBlockchainAddressChain :: !Chain
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec DestinationBlockchain)

instance HasCodec DestinationBlockchain where
  codec =
    object "DestinationBlockchain" $
      DestinationBlockchain
        <$> requiredField' "type" .= destinationBlockchainType
        <*> requiredField' "address" .= destinationBlockchainAddress
        <*> optionalField' "addressTag" .= destinationBlockchainAddressTag
        <*> requiredField' "chain" .= destinationBlockchainAddressChain

data Identity = Identity
  { identityType :: !IdentityType,
    identityName :: !Text,
    identityAddresses :: ![Address]
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec Identity)

instance HasCodec Identity where
  codec =
    object "Identity" $
      Identity
        <$> requiredField' "type" .= identityType
        <*> requiredField' "name" .= identityName
        <*> requiredField' "addresses" .= identityAddresses

data IdentityType = Individual | Business
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec IdentityType)

instance HasCodec IdentityType where
  codec = stringConstCodec $ NE.fromList [(Individual, "individual"), (Business, "business")]

data TransferType = Wallet | Blockchain
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec TransferType)

instance HasCodec TransferType where
  codec = stringConstCodec $ NE.fromList [(Wallet, "wallet"), (Blockchain, "blockchain")]

data TransferErrorCode
  = TransferInsufficientFunds
  | BlockchainError
  | TransferDenied
  | TransferFailed
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec TransferErrorCode)

instance HasCodec TransferErrorCode where
  codec =
    stringConstCodec $
      NE.fromList
        [ (TransferInsufficientFunds, "insufficient_funds"),
          (BlockchainError, "blockchain_error"),
          (TransferDenied, "transfer_denied"),
          (TransferFailed, "transfer_failed")
        ]

---------------------------------------------------------------
-- Address Types
---------------------------------------------------------------

data DepositAddressesRequest

type instance CircleRequest DepositAddressesRequest = CircleResponseBody [DepositAddressResponseBody]

data DepositAddressRequest

type instance CircleRequest DepositAddressRequest = CircleResponseBody DepositAddressResponseBody

data DepositAddressResponseBody = DepositAddressResponseBody
  { depositAddressResponseBodyAddress :: !HexString,
    depositAddressResponseBodyAddressTag :: !(Maybe Text), -- The docs say it's on the response, but the sandbox API doesn't return in. Make it a `Maybe` for now.
    depositAddressResponseBodyCurrency :: !SupportedCurrencies,
    depositAddressResponseBodyChain :: !Chain
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec DepositAddressResponseBody)

instance HasCodec DepositAddressResponseBody where
  codec =
    object "DepositAddressResponseBody" $
      DepositAddressResponseBody
        <$> requiredField' "address" .= depositAddressResponseBodyAddress
        <*> optionalField' "addressTag" .= depositAddressResponseBodyAddressTag
        <*> requiredField' "currency" .= depositAddressResponseBodyCurrency
        <*> requiredField' "chain" .= depositAddressResponseBodyChain

-- | Request body for creating a new deposit address
data DepositAddressRequestBody = DepositAddressRequestBody
  { depositAddressRequestBodyIdempotencyKey :: !UUID,
    depositAddressRequestBodyCurrency :: !SupportedCurrencies,
    depositAddressRequestBodyChain :: !Chain
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec DepositAddressRequestBody)

instance HasCodec DepositAddressRequestBody where
  codec =
    object "DepositAddressRequestBody" $
      DepositAddressRequestBody
        <$> requiredField' "idempotencyKey" .= depositAddressRequestBodyIdempotencyKey
        <*> requiredField' "currency" .= depositAddressRequestBodyCurrency
        <*> requiredField' "chain" .= depositAddressRequestBodyChain

data RecipientAddressesRequest

type instance CircleRequest RecipientAddressesRequest = CircleResponseBody [RecipientAddressResponseBody]

instance CircleHasParam RecipientAddressesRequest PaginationQueryParams

instance CircleHasParam RecipientAddressesRequest FromQueryParam

instance CircleHasParam RecipientAddressesRequest ToQueryParam

instance CircleHasParam RecipientAddressesRequest PageSizeQueryParam

data RecipientAddressRequest

type instance CircleRequest RecipientAddressRequest = CircleResponseBody RecipientAddressResponseBody

data RecipientAddressResponseBody = RecipientAddressResponseBody
  { recipientAddressResponseBodyId :: !UUID,
    recipientAddressResponseBodyAddress :: !HexString,
    recipientAddressResponseBodyAddressTag :: !(Maybe Text),
    recipientAddressResponseBodyChain :: !Chain,
    recipientAddressResponseBodyCurrency :: !SupportedCurrencies,
    recipientAddressResponseBodyDescription :: !Text
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec RecipientAddressResponseBody)

instance HasCodec RecipientAddressResponseBody where
  codec =
    object "RecipientAddressResponseBody" $
      RecipientAddressResponseBody
        <$> requiredField' "id" .= recipientAddressResponseBodyId
        <*> requiredField' "address" .= recipientAddressResponseBodyAddress
        <*> optionalField' "addressTag" .= recipientAddressResponseBodyAddressTag
        <*> requiredField' "chain" .= recipientAddressResponseBodyChain
        <*> requiredField' "currency" .= recipientAddressResponseBodyCurrency
        <*> requiredField' "description" .= recipientAddressResponseBodyDescription

-- | Request body for creating a new recipient address
data RecipientAddressRequestBody = RecipientAddressRequestBody
  { recipientAddressRequestBodyIdempotencyKey :: !UUID,
    recipientAddressRequestBodyAddress :: !HexString,
    recipientAddressRequestBodyAddressTag :: !(Maybe Text),
    recipientAddressRequestBodyChain :: !Chain,
    recipientAddressRequestBodyCurrency :: !SupportedCurrencies,
    recipientAddressRequestBodyDescription :: !Text
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec RecipientAddressRequestBody)

instance HasCodec RecipientAddressRequestBody where
  codec =
    object "RecipientAddressRequestBody" $
      RecipientAddressRequestBody
        <$> requiredField' "idempotencyKey" .= recipientAddressRequestBodyIdempotencyKey
        <*> requiredField' "address" .= recipientAddressRequestBodyAddress
        <*> optionalField' "addressTag" .= recipientAddressRequestBodyAddressTag
        <*> requiredField' "chain" .= recipientAddressRequestBodyChain
        <*> requiredField' "currency" .= recipientAddressRequestBodyCurrency
        <*> requiredField' "description" .= recipientAddressRequestBodyDescription

---------------------------------------------------------------
-- Deposit Types
---------------------------------------------------------------

data DepositsRequest

type instance CircleRequest DepositsRequest = CircleResponseBody [DepositResponseBody]

instance CircleHasParam DepositsRequest TypeQueryParam

instance CircleHasParam DepositsRequest PaginationQueryParams

instance CircleHasParam DepositsRequest FromQueryParam

instance CircleHasParam DepositsRequest ToQueryParam

instance CircleHasParam DepositsRequest PageSizeQueryParam

data DepositResponseBody = DepositResponseBody
  { depositResponseBodyId :: !UUID,
    depositResponseBodySourceWalletId :: !(Maybe WalletId),
    depositResponseBodyDestination :: !DestinationWallet,
    depositResponseBodyAmount :: !MoneyAmount,
    depositResponseBodyFee :: !MoneyAmount,
    depositResponseBodyStatus :: !Status,
    depositResponseBodyRiskEvaluation :: !(Maybe RiskEvaluation),
    depositResponseBodyCreateDate :: !UTCTime,
    depositResponseBodyUpdateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec DepositResponseBody)

instance HasCodec DepositResponseBody where
  codec =
    object "DepositResponseBody" $
      DepositResponseBody
        <$> requiredField' "id" .= depositResponseBodyId
        <*> optionalField' "sourceWalletId" .= depositResponseBodySourceWalletId
        <*> requiredField' "destination" .= depositResponseBodyDestination
        <*> requiredField' "amount" .= depositResponseBodyAmount
        <*> requiredField' "fee" .= depositResponseBodyFee
        <*> requiredField' "status" .= depositResponseBodyStatus
        <*> requiredField' "riskEvaluation" .= depositResponseBodyRiskEvaluation
        <*> requiredField' "createDate" .= depositResponseBodyCreateDate
        <*> requiredField' "updateDate" .= depositResponseBodyUpdateDate

---------------------------------------------------------------
-- Mock Payment Types
---------------------------------------------------------------

data MockPaymentRequest

type instance CircleRequest MockPaymentRequest = CircleResponseBody MockPaymentResponseBody

data MockPaymentResponseBody = MockPaymentResponseBody
  { mockPaymentResponseBodyTrackingRef :: !(Maybe TrackingReference),
    mockPaymentResponseBodyAmount :: !(Maybe MoneyAmount),
    mockPaymentResponseBodyBeneficiaryBank :: !(Maybe BeneficiaryBankDetails),
    mockPaymentResponseBodyStatus :: !(Maybe Status)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MockPaymentResponseBody)

instance HasCodec MockPaymentResponseBody where
  codec =
    object "MockPaymentResponseBody" $
      MockPaymentResponseBody
        <$> optionalField' "trackingRef" .= mockPaymentResponseBodyTrackingRef
        <*> optionalField' "amount" .= mockPaymentResponseBodyAmount
        <*> optionalField' "beneficiaryBank" .= mockPaymentResponseBodyBeneficiaryBank
        <*> optionalField' "status" .= mockPaymentResponseBodyStatus

-- | Request body to create a mock SEN or Wire payment (in the sandbox only).
data MockSenOrWirePaymentRequestBody = MockSenOrWirePaymentRequestBody
  { mockSenOrWirePaymentRequestBodyTrackingRef :: !TrackingReference,
    mockSenOrWirePaymentRequestBodyAmount :: !MoneyAmount,
    mockSenOrWirePaymentRequestBodyBeneficiaryBank :: !MockBeneficiaryBankDetails
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MockSenOrWirePaymentRequestBody)

instance HasCodec MockSenOrWirePaymentRequestBody where
  codec =
    object "MockSenOrWirePaymentRequestBody" $
      MockSenOrWirePaymentRequestBody
        <$> requiredField' "trackingRef" .= mockSenOrWirePaymentRequestBodyTrackingRef
        <*> requiredField' "amount" .= mockSenOrWirePaymentRequestBodyAmount
        <*> requiredField' "beneficiaryBank" .= mockSenOrWirePaymentRequestBodyBeneficiaryBank

-- | Request body to create a mock SEPA payment (in the sandbox only).
data MockSEPAPaymentRequestBody = MockSEPAPaymentRequestBody
  { mockSEPAPaymentRequestBodyTrackingRef :: !TrackingReference,
    mockSEPAPaymentRequestBodyAmount :: !MoneyAmount
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MockSEPAPaymentRequestBody)

instance HasCodec MockSEPAPaymentRequestBody where
  codec =
    object "MockSEPAPaymentRequestBody" $
      MockSEPAPaymentRequestBody
        <$> requiredField' "trackingRef" .= mockSEPAPaymentRequestBodyTrackingRef
        <*> requiredField' "amount" .= mockSEPAPaymentRequestBodyAmount

newtype MockBeneficiaryBankDetails = MockBeneficiaryBankDetails {mockBeneficiaryBankDetailsAccountNumber :: AccountNumber}
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec MockBeneficiaryBankDetails where
  codec = dimapCodec MockBeneficiaryBankDetails mockBeneficiaryBankDetailsAccountNumber codec

---------------------------------------------------------------
-- Silvergate SEN Types
---------------------------------------------------------------

data SENAccountRequest

type instance CircleRequest SENAccountRequest = CircleResponseBody SENAccountResponseBody

data SENAccountsRequest

type instance CircleRequest SENAccountsRequest = CircleResponseBody [SENAccountResponseBody]

data SENInstructionsRequest

type instance CircleRequest SENInstructionsRequest = CircleResponseBody SENInstructionsResponseData

-- | Request body to create a Silvergate SEN account.
data SENAccountRequestBody = SENAccountRequestBody
  { senAccountRequestBodyIdempotencyKey :: !UUID,
    senAccountRequestBodyAccountNumber :: !AccountNumber,
    senAccountRequestBodyCurrency :: !(Maybe SupportedCurrencies)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SENAccountRequestBody)

instance HasCodec SENAccountRequestBody where
  codec =
    object "SENAccountRequestBody" $
      SENAccountRequestBody
        <$> requiredField' "idempotencyKey" .= senAccountRequestBodyIdempotencyKey
        <*> requiredField' "accountNumber" .= senAccountRequestBodyAccountNumber
        <*> optionalField' "currency" .= senAccountRequestBodyCurrency

data SENAccountResponseBody = SENAccountResponseBody
  { senAccountResponseBodyId :: !UUID,
    senAccountResponseBodyStatus :: !Status,
    senAccountResponseBodyDescription :: !Text, -- TODO better type: Bank name plus last four digits of the bank account number or IBAN.  Make a custom type for this
    senAccountResponseBodyTrackingRef :: !TrackingReference,
    senAccountResponseBodyCreateDate :: !UTCTime,
    senAccountResponseBodyUpdateDate :: !UTCTime,
    senAccountResponseBodyCurrency :: !(Maybe SupportedCurrencies)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SENAccountResponseBody)

instance HasCodec SENAccountResponseBody where
  codec =
    object "SENAccountResponseBody" $
      SENAccountResponseBody
        <$> requiredField' "id" .= senAccountResponseBodyId
        <*> requiredField' "status" .= senAccountResponseBodyStatus
        <*> requiredField' "description" .= senAccountResponseBodyDescription
        <*> requiredField' "trackingRef" .= senAccountResponseBodyTrackingRef
        <*> requiredField' "createDate" .= senAccountResponseBodyCreateDate
        <*> requiredField' "updateDate" .= senAccountResponseBodyUpdateDate
        <*> optionalField' "currency" .= senAccountResponseBodyCurrency

data SENInstructionsResponseData = SENInstructionsResponseData
  { senInstructionsResponseDataTrackingRef :: !TrackingReference,
    senInstructionsResponseDataAccountNumber :: !AccountNumber,
    senInstructionsResponseDataCurrency :: !SupportedCurrencies
  }
  deriving stock (Eq, Show, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SENInstructionsResponseData)

instance HasCodec SENInstructionsResponseData where
  codec =
    object "SENInstructionsResponseData" $
      SENInstructionsResponseData
        <$> requiredField' "trackingRef" .= senInstructionsResponseDataTrackingRef
        <*> requiredField' "accountNumber" .= senInstructionsResponseDataAccountNumber
        <*> requiredField' "currency" .= senInstructionsResponseDataCurrency

---------------------------------------------------------------
-- Signet Bank Account Types
---------------------------------------------------------------

data SignetBankAccountRequest

type instance CircleRequest SignetBankAccountRequest = CircleResponseBody SignetBankAccountResponseData

data SignetBankAccountsRequest

type instance CircleRequest SignetBankAccountsRequest = CircleResponseBody [SignetBankAccountResponseData]

data SignetBankInstructionsRequest

type instance CircleRequest SignetBankInstructionsRequest = CircleResponseBody SignetBankInstructionsResponseData

-- | Request body to create Signet Bank bank account.
data SignetBankAccountRequestBody = SignetBankAccountRequestBody
  { signetBankAccountRequestBodyIdempotencyKey :: !UUID,
    signetBankAccountRequestBodyWalletAddress :: !HexString
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SignetBankAccountRequestBody)

instance HasCodec SignetBankAccountRequestBody where
  codec =
    object "SignetBankAccountRequestBody" $
      SignetBankAccountRequestBody
        <$> requiredField' "idempotencyKey" .= signetBankAccountRequestBodyIdempotencyKey
        <*> requiredField' "walletAddress" .= signetBankAccountRequestBodyWalletAddress

data SignetBankAccountResponseData = SignetBankAccountResponseData
  { signetBankAccountId :: !UUID,
    signetBankAccountStatus :: !Status,
    signetBankAccountTrackingRef :: !TrackingReference,
    signetBankAccountWalletAddress :: !HexString,
    signetBankAccountCreateDate :: !UTCTime,
    signetBankAccountUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SignetBankAccountResponseData)

instance HasCodec SignetBankAccountResponseData where
  codec =
    object "SignetBankAccountResponseData" $
      SignetBankAccountResponseData
        <$> requiredField' "id" .= signetBankAccountId
        <*> requiredField' "status" .= signetBankAccountStatus
        <*> requiredField' "trackingRef" .= signetBankAccountTrackingRef
        <*> requiredField' "walletAddress" .= signetBankAccountWalletAddress
        <*> requiredField' "createDate" .= signetBankAccountCreateDate
        <*> requiredField' "updateDate" .= signetBankAccountUpdateDate

data SignetBankInstructionsResponseData = SignetBankInstructionsResponseData
  { signetBankInstructionsTrackingRef :: !(Maybe TrackingReference),
    signetBankInstructionsWalletAddress :: !(Maybe HexString)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SignetBankInstructionsResponseData)

instance HasCodec SignetBankInstructionsResponseData where
  codec =
    object "SignetBankInstructionsResponseData" $
      SignetBankInstructionsResponseData
        <$> optionalField' "trackingRef" .= signetBankInstructionsTrackingRef
        <*> optionalField' "walletAddress" .= signetBankInstructionsWalletAddress

---------------------------------------------------------------
-- Wire Account Types
---------------------------------------------------------------

data WireAccountRequest

type instance CircleRequest WireAccountRequest = CircleResponseBody WireAccountResponseBody

data WireAccountsRequest

type instance CircleRequest WireAccountsRequest = CircleResponseBody [WireAccountResponseBody]

data WireInstructionsRequest

type instance CircleRequest WireInstructionsRequest = CircleResponseBody WireInstructionsResponseData

instance CircleHasParam WireInstructionsRequest PaginationQueryParams

-- | Request body to create a wire account.  Sum type because this endpoint supports several
-- different types of wire accounts.
data WireAccountRequestBody
  = USBankAccount !USBankAccountRequestBody
  | IBANBankAccount !IBANBankAccountRequestBody
  | NonIBANBankAccount !NonIBANBankAccountRequestBody
  deriving (Eq, Show)

data USBankAccountRequestBody = USBankAccountRequestBody
  { usBankAccountIdempotencyKey :: !UUID,
    usBankAccountAccountNumber :: !AccountNumber,
    usBankAccountRoutingNumber :: !RoutingNumber,
    usBankAccountBillingDetails :: !BillingDetails,
    usBankAccountBankAddress :: !BankAddress
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec USBankAccountRequestBody)

instance HasCodec USBankAccountRequestBody where
  codec =
    object "USBankAccountRequestBody" $
      USBankAccountRequestBody
        <$> requiredField' "idempotencyKey" .= usBankAccountIdempotencyKey
        <*> requiredField' "accountNumber" .= usBankAccountAccountNumber
        <*> requiredField' "routingNumber" .= usBankAccountRoutingNumber
        <*> requiredField' "billingDetails" .= usBankAccountBillingDetails
        <*> requiredField' "bankAddress" .= usBankAccountBankAddress

data IBANBankAccountRequestBody = IBANBankAccountRequestBody
  { ibanBankAccountIdempotencyKey :: !UUID,
    ibanBankAccountIBAN :: !Iban,
    ibanBankAccountBillingDetails :: !BillingDetails,
    ibanBankAccountBankAddress :: !BankAddress
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec IBANBankAccountRequestBody)

instance HasCodec IBANBankAccountRequestBody where
  codec =
    object "IBANBankAccountRequestBody" $
      IBANBankAccountRequestBody
        <$> requiredField' "idempotencyKey" .= ibanBankAccountIdempotencyKey
        <*> requiredField' "iban" .= ibanBankAccountIBAN
        <*> requiredField' "billingDetails" .= ibanBankAccountBillingDetails
        <*> requiredField' "bankAddress" .= ibanBankAccountBankAddress

data NonIBANBankAccountRequestBody = NonIBANBankAccountRequestBody
  { nonIBANBankAccountIdempotencyKey :: !UUID,
    nonIBANBankAccountAccountNumber :: !AccountNumber,
    nonIBANBankAccountRoutingNumber :: !RoutingNumber,
    nonIBANBankAccountBillingDetails :: !BillingDetails,
    nonIBANBankAccountBankAddress :: !BankAddress
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec NonIBANBankAccountRequestBody)

instance HasCodec NonIBANBankAccountRequestBody where
  codec =
    object "NonIBANBankAccountRequestBody" $
      NonIBANBankAccountRequestBody
        <$> requiredField' "idempotencyKey" .= nonIBANBankAccountIdempotencyKey
        <*> requiredField' "accountNumber" .= nonIBANBankAccountAccountNumber
        <*> requiredField' "routingNumber" .= nonIBANBankAccountRoutingNumber
        <*> requiredField' "billingDetails" .= nonIBANBankAccountBillingDetails
        <*> requiredField' "bankAddress" .= nonIBANBankAccountBankAddress

data WireInstructionsResponseData = WireInstructionsResponseData
  { wireInstructionsResponseDataTrackingRef :: !TrackingReference,
    wireInstructionsResponseDataBeneficiaryDetails :: !BeneficiaryDetails,
    wireInstructionsResponseDataBeneficiaryBankDetails :: !BeneficiaryBankDetails
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec WireInstructionsResponseData)

instance HasCodec WireInstructionsResponseData where
  codec =
    object "WireInstructionsResponseData" $
      WireInstructionsResponseData
        <$> requiredField' "trackingRef" .= wireInstructionsResponseDataTrackingRef
        <*> requiredField' "beneficiary" .= wireInstructionsResponseDataBeneficiaryDetails
        <*> requiredField' "beneficiaryBank" .= wireInstructionsResponseDataBeneficiaryBankDetails

data WireAccountResponseBody = WireAccountResponseBody
  { wireAccountResponseBodyId :: !UUID,
    wireAccountResponseBodyStatus :: !Status,
    wireAccountResponseBodyDescription :: !Text, -- TODO better type: Bank name plus last four digits of the bank account number or IBAN.  Make a custom type for this
    wireAccountResponseBodyTrackingRef :: !TrackingReference,
    wireAccountResponseBodyFingerprint :: !UUID,
    wireAccountResponseBodyBillingDetails :: !BillingDetails,
    wireAccountResponseBodyBankAddress :: !BankAddress,
    wireAccountResponseBodyCreateDate :: !UTCTime,
    wireAccountResponseBodyUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec WireAccountResponseBody)

instance HasCodec WireAccountResponseBody where
  codec =
    object "WireAccountResponseBody" $
      WireAccountResponseBody
        <$> requiredField' "id" .= wireAccountResponseBodyId
        <*> requiredField' "status" .= wireAccountResponseBodyStatus
        <*> requiredField' "description" .= wireAccountResponseBodyDescription
        <*> requiredField' "trackingRef" .= wireAccountResponseBodyTrackingRef
        <*> requiredField' "fingerprint" .= wireAccountResponseBodyFingerprint
        <*> requiredField' "billingDetails" .= wireAccountResponseBodyBillingDetails
        <*> requiredField' "bankAddress" .= wireAccountResponseBodyBankAddress
        <*> requiredField' "createDate" .= wireAccountResponseBodyCreateDate
        <*> requiredField' "updateDate" .= wireAccountResponseBodyUpdateDate

-- Payments API
-- this could maybe be a new module?  IDK.

---------------------------------------------------------------
-- Payment Types
---------------------------------------------------------------

data PaymentRequest

type instance CircleRequest PaymentRequest = CircleResponseBody (ThisOrThat FiatOrCryptoPaymentResponseBody FiatCancelOrRefundResponseBody)

data PaymentsRequest

type instance CircleRequest PaymentsRequest = CircleResponseBody [ThisOrThat FiatOrCryptoPaymentResponseBody FiatCancelOrRefundResponseBody]

instance CircleHasParam PaymentsRequest PaginationQueryParams

instance CircleHasParam PaymentsRequest FromQueryParam

instance CircleHasParam PaymentsRequest ToQueryParam

instance CircleHasParam PaymentsRequest PageSizeQueryParam

instance CircleHasParam PaymentsRequest PaymentStatusQueryParams

instance CircleHasParam PaymentsRequest TypeQueryParam

instance CircleHasParam PaymentsRequest DestinationQueryParam

instance CircleHasParam PaymentsRequest SourceQueryParam

instance CircleHasParam PaymentsRequest SettlementIdQueryParam

instance CircleHasParam PaymentsRequest PaymentIntentIdQueryParam

-- | Request body to create any kind of payment.
data CreatePaymentRequestBody = CreatePaymentRequestBody
  { createPaymentIdempotencyKey :: !UUID,
    createPaymentKeyId :: !Text, -- TODO this is actually a UUID, but in Sandbox it has to be `key1`.  Figure out how to reconcile this later.
    requestMetadata :: !RequestMetadata,
    createPaymentAmount :: !MoneyAmount,
    createPaymentAutoCapture :: !(Maybe Bool),
    createPaymentVerification :: !VerificationType,
    -- | The following two fields are only present if VerificationType = ThreeDSecure
    createPaymentVerificationSuccessUrl :: !(Maybe URL),
    createPaymentVerificationFailureUrl :: !(Maybe URL),
    createPaymentSource :: !PaymentSource,
    createPaymentDescription :: !(Maybe Text),
    -- | This encrypted data needs to encrypt the card CVV
    createPaymentEncryptedData :: !(Maybe Text),
    createPaymentChannel :: !(Maybe UUID)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreatePaymentRequestBody)

instance HasCodec CreatePaymentRequestBody where
  codec =
    object "CreatePaymentRequestBody" $
      CreatePaymentRequestBody
        <$> requiredField' "idempotencyKey" .= createPaymentIdempotencyKey
        <*> requiredField' "keyId" .= createPaymentKeyId
        <*> requiredField' "metadata" .= requestMetadata
        <*> requiredField' "amount" .= createPaymentAmount
        <*> optionalField' "autoCapture" .= createPaymentAutoCapture
        <*> requiredField' "verification" .= createPaymentVerification
        <*> optionalField' "verificationSuccessfulUrl" .= createPaymentVerificationSuccessUrl
        <*> optionalField' "verificationFailureUrl" .= createPaymentVerificationFailureUrl
        <*> requiredField' "source" .= createPaymentSource
        <*> optionalField' "description" .= createPaymentDescription
        <*> optionalField' "encryptedData" .= createPaymentEncryptedData
        <*> optionalField' "channel" .= createPaymentChannel

data RequestMetadata = RequestMetadata
  { requestMetadataEmail :: !Email,
    requestMetadataPhoneNumber :: !(Maybe PhoneNumber),
    requestMetadataSessionId :: !SessionId,
    requestMetadataIpAddress :: !IPAddress
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec RequestMetadata)

newtype SessionId = SessionId
  { unSessionId :: Text -- TODO consider validating if necessary
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec SessionId where
  codec = dimapCodec SessionId unSessionId codec

newtype IPAddress = IPAddress
  { unIPAddress :: Text -- TODO consider validating if necessary
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec IPAddress where
  codec = dimapCodec IPAddress unIPAddress codec

newtype PhoneNumber = PhoneNumber
  { unPhoneNumber :: Text -- TODO consider validating if necessary
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec PhoneNumber where
  codec = dimapCodec PhoneNumber unPhoneNumber codec

instance HasCodec RequestMetadata where
  codec =
    object "RequestMetadata" $
      RequestMetadata
        <$> requiredField' "email" .= requestMetadataEmail
        <*> optionalField' "phoneNumber" .= requestMetadataPhoneNumber
        <*> requiredField' "sessionId" .= requestMetadataSessionId
        <*> requiredField' "ipAddress" .= requestMetadataIpAddress

data PaymentErrorCode
  = PaymentFailedErrorCode
  | PaymentFraudDetected
  | PaymentDenied
  | PaymentNotSupportedByIssuer
  | PaymentNotFunded
  | PaymentUnprocessable
  | PaymentStoppedByIssuer
  | PaymentCanceled
  | PaymentReturned
  | PaymentFailedBalanceCheck
  | CardFailed
  | CardInvalid
  | CardAddressMismatch
  | CardZipMismatch
  | CardCvvInvalid
  | CardExpired
  | CardLimitViolated
  | CardNotHonored
  | CardCvvRequired
  | CardRestricted
  | CardAccountIneligible
  | CardNetworkUnsupported
  | ChannelInvalid
  | UnauthorizedTransaction
  | BankAccountIneligible
  | PaymentBankTransactionError
  | InvalidAccountNumber
  | InvalidWireRtn
  | InvalidAchRtn
  | RefIdInvalid
  | AccountNameMismatch
  | AccountNumberMismatch
  | AccountIneligible
  | WalletAddressMismatch
  | CustomerNameMismatch
  | InstitutionNameMismatch
  | PaymentVendorInactive
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentErrorCode)

instance HasCodec PaymentErrorCode where
  codec =
    stringConstCodec $
      NE.fromList
        [ (PaymentFailedErrorCode, "payment_failed"),
          (PaymentFraudDetected, "payment_fraud_detected"),
          (PaymentDenied, "payment_denied"),
          (PaymentNotSupportedByIssuer, "payment_not_supported_by_issuer"),
          (PaymentNotFunded, "payment_not_funded"),
          (PaymentUnprocessable, "payment_unprocessable"),
          (PaymentStoppedByIssuer, "payment_stopped_by_issuer"),
          (PaymentCanceled, "payment_canceled"),
          (PaymentReturned, "payment_returned"),
          (PaymentFailedBalanceCheck, "payment_failed_balance_check"),
          (CardFailed, "card_failed"),
          (CardInvalid, "card_invalid"),
          (CardAddressMismatch, "card_address_mismatch"),
          (CardZipMismatch, "card_zip_mismatch"),
          (CardCvvInvalid, "card_cvv_invalid"),
          (CardExpired, "card_expired"),
          (CardLimitViolated, "card_limit_violated"),
          (CardNotHonored, "card_not_honored"),
          (CardCvvRequired, "card_cvv_required"),
          (CardRestricted, "card_restricted"),
          (CardAccountIneligible, "card_account_ineligible"),
          (CardNetworkUnsupported, "card_network_unsupported"),
          (ChannelInvalid, "channel_invalid"),
          (UnauthorizedTransaction, "unauthorized_transaction"),
          (BankAccountIneligible, "bank_account_ineligible"),
          (PaymentBankTransactionError, "bank_transaction_error"),
          (InvalidAccountNumber, "invalid_account_number"),
          (InvalidWireRtn, "invalid_wire_rtn"),
          (InvalidAchRtn, "invalid_ach_rtn"),
          (RefIdInvalid, "ref_id_invalid"),
          (AccountNameMismatch, "account_name_mismatch"),
          (AccountNumberMismatch, "account_number_mismatch"),
          (AccountIneligible, "account_ineligible"),
          (WalletAddressMismatch, "wallet_address_mismatch"),
          (CustomerNameMismatch, "customer_name_mismatch"),
          (InstitutionNameMismatch, "institution_name_mismatch"),
          (PaymentVendorInactive, "vendor_inactive")
        ]

-- TODO could likely make a better abstraction here.

-- | A FiatOrCryptoPaymentResponseBody object represents a fiat or crypto payment.  These payments look identical
-- except for the "Description" field, and the fact that a FiatPayment could have response verification data, whereas
-- a crypto payment could have info about the deposit address, transaction hash etc.
-- I'd love to differentiate these fields based on what I can parse from JSON, but there's enough overlap between
-- the two response bodies that I can cheat for now.
data FiatOrCryptoPaymentResponseBody = FiatOrCryptoPaymentResponseBody
  { -- the following fields will be present on every response
    fiatOrCryptoPaymentId :: !UUID,
    fiatOrCryptoPaymentType :: !PaymentType,
    fiatOrCryptoPaymentMerchantId :: !UUID,
    fiatOrCryptoPaymentMerchantWalletId :: !WalletId,
    fiatOrCryptoPaymentAmount :: !MoneyAmount,
    fiatOrCryptoPaymentSource :: !PaymentSource,
    fiatOrCryptoPaymentDescription :: !Text,
    fiatOrCryptoPaymentStatus :: !PaymentStatus,
    -- the following fields will only be present on Crypto payments
    fiatOrCryptoPaymentPaymentIntentId :: !(Maybe UUID),
    fiatOrCryptoPaymentSettlementAmount :: !(Maybe MoneyAmount),
    fiatOrCryptoPaymentDepositAddress :: !(Maybe PaymentDepositAddress),
    fiatOrCryptoPaymentTransactionHash :: !(Maybe HexString),
    -- the following fields will only be present on fiat payments
    fiatOrCryptoPaymentVerification :: !(Maybe VerificationData),
    fiatOrCryptoPaymentCaptured :: !(Maybe Bool),
    fiatOrCryptoPaymentCaptureAmount :: !(Maybe MoneyAmount),
    fiatOrCryptoPaymentCaptureDate :: !(Maybe UTCTime),
    fiatOrCryptoPaymentRequiredAction :: !(Maybe PaymentActionRequired),
    fiatOrCryptoPaymentCancel :: !(Maybe FiatCancelOrRefundResponseBody),
    fiatOrCryptoPaymentRefunds :: !(Maybe [FiatCancelOrRefundResponseBody]),
    fiatOrCryptoPaymentFees :: !(Maybe MoneyAmount),
    fiatOrCryptoPaymentChannel :: !(Maybe UUID),
    fiatOrCryptoPaymentCreateDate :: !(Maybe UTCTime),
    fiatOrCryptoPaymentUpdateDate :: !(Maybe UTCTime),
    fiatOrCryptoPaymentTrackingRef :: !(Maybe TrackingReference),
    fiatOrCryptoPaymentErrorCode :: !(Maybe PaymentErrorCode),
    fiatOrCryptoMetadata :: !(Maybe ResponseMetadata),
    fiatOrCryptoPaymentRiskEvaluation :: !(Maybe RiskEvaluation)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec FiatOrCryptoPaymentResponseBody)

instance HasCodec FiatOrCryptoPaymentResponseBody where
  codec =
    object "FiatOrCryptoPaymentResponseBody" $
      FiatOrCryptoPaymentResponseBody
        <$> requiredField' "id" .= fiatOrCryptoPaymentId
        <*> requiredField' "type" .= fiatOrCryptoPaymentType
        <*> requiredField' "merchantId" .= fiatOrCryptoPaymentMerchantId
        <*> requiredField' "merchantWalletId" .= fiatOrCryptoPaymentMerchantWalletId
        <*> requiredField' "amount" .= fiatOrCryptoPaymentAmount
        <*> requiredField' "source" .= fiatOrCryptoPaymentSource
        <*> requiredField' "description" .= fiatOrCryptoPaymentDescription
        <*> requiredField' "status" .= fiatOrCryptoPaymentStatus
        <*> optionalField' "paymentIntentId" .= fiatOrCryptoPaymentPaymentIntentId
        <*> optionalField' "settlementAmount" .= fiatOrCryptoPaymentSettlementAmount
        <*> optionalField' "depositAddress" .= fiatOrCryptoPaymentDepositAddress
        <*> optionalField' "transactionHash" .= fiatOrCryptoPaymentTransactionHash
        <*> optionalField' "verification" .= fiatOrCryptoPaymentVerification
        <*> optionalField' "captured" .= fiatOrCryptoPaymentCaptured
        <*> optionalField' "captureAmount" .= fiatOrCryptoPaymentCaptureAmount
        <*> optionalField' "captureDate" .= fiatOrCryptoPaymentCaptureDate
        <*> optionalField' "requiredAction" .= fiatOrCryptoPaymentRequiredAction
        <*> optionalField' "cancel" .= fiatOrCryptoPaymentCancel
        <*> optionalField' "refunds" .= fiatOrCryptoPaymentRefunds
        <*> optionalField' "fees" .= fiatOrCryptoPaymentFees
        <*> optionalField' "channel" .= fiatOrCryptoPaymentChannel
        <*> optionalField' "createDate" .= fiatOrCryptoPaymentCreateDate
        <*> optionalField' "updateDate" .= fiatOrCryptoPaymentUpdateDate
        <*> optionalField' "trackingRef" .= fiatOrCryptoPaymentTrackingRef
        <*> optionalField' "errorCode" .= fiatOrCryptoPaymentErrorCode
        <*> optionalField' "metadata" .= fiatOrCryptoMetadata
        <*> optionalField' "channel" .= fiatOrCryptoPaymentRiskEvaluation

data ResponseMetadata = ResponseMetadata
  { responseMetadataEmail :: !Email,
    responseMetadataPhoneNumber :: !(Maybe PhoneNumber)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ResponseMetadata)

instance HasCodec ResponseMetadata where
  codec =
    object "ResponseMetadata" $
      ResponseMetadata
        <$> requiredField' "email" .= responseMetadataEmail
        <*> optionalField' "phoneNumber" .= responseMetadataPhoneNumber

data VerificationData = VerificationData
  { verificationAVS :: !AVS,
    verificationCVV :: !CVV
  }
  deriving (Eq, Show, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec VerificationData)

instance HasCodec VerificationData where
  codec =
    object "VerificationData" $
      VerificationData
        <$> requiredField "avs" "Represents the raw AVS response, expressed as an upper-case letter." .= verificationAVS
        <*> requiredField "cvv" "Represents the CVV response" .= verificationCVV

data AVS = AVSNotRequested | AVSPending | Y | N
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec AVS)

instance HasCodec AVS where
  codec = stringConstCodec $ NE.fromList [(AVSNotRequested, "not_requested"), (AVSPending, "pending"), (Y, "Y"), (N, "N")]

data CVV = CVVNotRequested | CVVPass | CVVFail | CVVUnavailable | CVVPending
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CVV)

instance HasCodec CVV where
  codec = stringConstCodec $ NE.fromList [(CVVNotRequested, "not_requested"), (CVVPending, "pending"), (CVVPass, "pass"), (CVVFail, "fail"), (CVVUnavailable, "unavailable")]

data PaymentDepositAddress = PaymentDepositAddress
  { paymentDepositAddressChain :: !Chain,
    paymentDepositAddressAddress :: !HexString
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentDepositAddress)

instance HasCodec PaymentDepositAddress where
  codec =
    object "PaymentDepositAddress" $
      PaymentDepositAddress
        <$> requiredField' "chain" .= paymentDepositAddressChain
        <*> requiredField' "address" .= paymentDepositAddressAddress

-- | A FiatCancelOrRefundResponseBody object represents an attempt at canceling or refunding a payment.
-- Cancellations apply only to card payments, and its presence doesn't necessarily mean that the cancellation was successful.
-- A successful cancellation has a status of paid, a successful refund has a status of confirmed.
data FiatCancelOrRefundResponseBody = FiatCancelOrRefundResponseBody
  { -- TODO I could likely do some better data modeling here, there's a ton of shared fields between these
    -- types so I kinda cheated and just made one mega type with maybes, but it'll be more ergonomic for devs
    -- to have a specific type that's generated from the parsing.  The tricky part is the differentiator is the
    -- field `type`, so I think I'll need to be clever about this.
    fiatCancelOrRefundResponseBodyId :: !UUID,
    fiatCancelOrRefundResponseBodyType :: !PaymentType,
    fiatCancelOrRefundResponseBodyMerchantId :: !UUID,
    fiatCancelOrRefundResponseBodyMerchantWalletId :: !WalletId,
    fiatCancelOrRefundResponseBodyAmount :: !MoneyAmount,
    fiatCancelOrRefundResponseBodySource :: !PaymentSource,
    fiatCancelOrRefundResponseBodyDescription :: !Text,
    fiatCancelOrRefundResponseBodyStatus :: !PaymentStatus,
    fiatCancelOrRefundResponseBodyOriginalPayment :: !OriginalFiatPayment,
    fiatCancelOrRefundResponseBodyFees :: !(Maybe MoneyAmount),
    fiatCancelOrRefundResponseBodyChannel :: !(Maybe Text),
    fiatCancelOrRefundResponseBodyReason :: !(Maybe CancelPaymentReason),
    fiatCancelOrRefundResponseBodyCreateDate :: !UTCTime,
    fiatCancelOrRefundResponseBodyUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec FiatCancelOrRefundResponseBody)

instance HasCodec FiatCancelOrRefundResponseBody where
  codec =
    object "FiatCancelOrRefundResponseBody" $
      FiatCancelOrRefundResponseBody
        <$> requiredField' "id" .= fiatCancelOrRefundResponseBodyId
        <*> requiredField' "type" .= fiatCancelOrRefundResponseBodyType
        <*> requiredField' "merchantId" .= fiatCancelOrRefundResponseBodyMerchantId
        <*> requiredField' "merchantWalletId" .= fiatCancelOrRefundResponseBodyMerchantWalletId
        <*> requiredField' "amount" .= fiatCancelOrRefundResponseBodyAmount
        <*> requiredField' "source" .= fiatCancelOrRefundResponseBodySource
        <*> requiredField' "description" .= fiatCancelOrRefundResponseBodyDescription
        <*> requiredField' "status" .= fiatCancelOrRefundResponseBodyStatus
        <*> requiredField' "originalPayment" .= fiatCancelOrRefundResponseBodyOriginalPayment
        <*> optionalField' "fees" .= fiatCancelOrRefundResponseBodyFees
        <*> optionalField' "channel" .= fiatCancelOrRefundResponseBodyChannel
        <*> optionalField' "reason" .= fiatCancelOrRefundResponseBodyReason
        <*> requiredField' "createDate" .= fiatCancelOrRefundResponseBodyCreateDate
        <*> requiredField' "updateDate" .= fiatCancelOrRefundResponseBodyUpdateDate

data OriginalFiatPayment = OriginalFiatPayment
  { originalFiatPaymentId :: !UUID,
    originalFiatPaymentType :: !PaymentType,
    originalFiatPaymentStatus :: !PaymentStatus,
    originalFiatPaymentCreateDate :: !UTCTime,
    originalFiatPaymentUpdateDate :: !UTCTime,
    originalFiatPaymentDescription :: !(Maybe Text),
    originalFiatPaymentAmount :: !(Maybe MoneyAmount),
    originalFiatPaymentFees :: !(Maybe MoneyAmount),
    originalFiatPaymentMerchantId :: !(Maybe UUID),
    originalFiatPaymentMerchantWalletId :: !(Maybe WalletId),
    originalFiatPaymentSource :: !(Maybe PaymentSource),
    originalFiatPaymentTrackingRef :: !(Maybe TrackingReference)
  }
  deriving (Eq, Show, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec OriginalFiatPayment)

instance HasCodec OriginalFiatPayment where
  codec =
    object "OriginalFiatPayment" $
      OriginalFiatPayment
        <$> requiredField' "id" .= originalFiatPaymentId
        <*> requiredField' "type" .= originalFiatPaymentType
        <*> requiredField' "status" .= originalFiatPaymentStatus
        <*> requiredField' "createDate" .= originalFiatPaymentCreateDate
        <*> requiredField' "updateDate" .= originalFiatPaymentUpdateDate
        <*> optionalField' "description" .= originalFiatPaymentDescription
        <*> optionalField' "amount" .= originalFiatPaymentAmount
        <*> optionalField' "fees" .= originalFiatPaymentFees
        <*> optionalField' "merchantId" .= originalFiatPaymentMerchantId
        <*> optionalField' "merchantWalletId" .= originalFiatPaymentMerchantWalletId
        <*> optionalField' "source" .= originalFiatPaymentSource
        <*> optionalField' "trackingRef" .= originalFiatPaymentTrackingRef

data PaymentSource = PaymentSource
  { paymentSourceId :: !UUID,
    paymentSourceType :: !PaymentSourceType
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentSource)

instance HasCodec PaymentSource where
  codec =
    object "PaymentSource" $
      PaymentSource
        <$> requiredField' "id" .= paymentSourceId
        <*> requiredField' "type" .= paymentSourceType

data PaymentActionRequired = PaymentActionRequired
  { paymentActionRequiredType :: !ActionRequiredType,
    paymentActionRequiredRedirectUrl :: !URL
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentActionRequired)

instance HasCodec PaymentActionRequired where
  codec =
    object "PaymentActionRequired" $
      PaymentActionRequired
        <$> requiredField' "type" .= paymentActionRequiredType
        <*> requiredField' "redirectUrl" .= paymentActionRequiredRedirectUrl

data ActionRequiredType = ThreeDSecureRequired
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ActionRequiredType)

instance HasCodec ActionRequiredType where
  codec = stringConstCodec $ NE.fromList [(ThreeDSecureRequired, "three_d_secure_required")]

data VerificationType = VerificationThreeDSecure | VerificationCVV
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec VerificationType)

instance HasCodec VerificationType where
  codec = stringConstCodec $ NE.fromList [(VerificationThreeDSecure, "three_d_secure"), (VerificationCVV, "cvv")]

data PaymentType = Payment | Cancel | Refund
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentType)

instance HasCodec PaymentType where
  codec = stringConstCodec $ NE.fromList [(Payment, "payment"), (Cancel, "cancel"), (Refund, "refund")]

data PaymentSourceType = Card | ACH | WireSource | SEPA
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentSourceType)

instance HasCodec PaymentSourceType where
  codec = stringConstCodec $ NE.fromList [(Card, "card"), (ACH, "ach"), (WireSource, "wire"), (SEPA, "sepa")]

-- | Request body to cancel a fiat payment.
data CancelPaymentRequestBody = CancelPaymentRequestBody
  { cancelPaymentIdempotencyKey :: !UUID,
    cancelPaymentReason :: !(Maybe CancelPaymentReason)
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec CancelPaymentRequestBody)

instance HasCodec CancelPaymentRequestBody where
  codec =
    object "CancelPaymentRequestBody" $
      CancelPaymentRequestBody
        <$> requiredField' "idempotencyKey" .= cancelPaymentIdempotencyKey
        <*> optionalField' "reason" .= cancelPaymentReason

data CancelPaymentReason
  = CancelPaymentReasonDuplicate
  | CancelPaymentReasonFraudulent
  | CancelPaymentReasonRequestedByCustomer
  | CancelPaymentReasonBankTransactionError
  | CancelPaymentReasonInvalidAccountNumber
  | CancelPaymentReasonInsufficientFunds
  | CancelPaymentReasonPaymentStoppedByIssuer
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CancelPaymentReason)

instance HasCodec CancelPaymentReason where
  codec =
    stringConstCodec $
      NE.fromList
        [ (CancelPaymentReasonDuplicate, "duplicate"),
          (CancelPaymentReasonFraudulent, "fraudulent"),
          (CancelPaymentReasonRequestedByCustomer, "requested_by_customer"),
          (CancelPaymentReasonBankTransactionError, "bank_transaction_error"),
          (CancelPaymentReasonInvalidAccountNumber, "invalid_account_number"),
          (CancelPaymentReasonInsufficientFunds, "insufficient_funds"),
          (CancelPaymentReasonPaymentStoppedByIssuer, "payment_stopped_by_issuer")
        ]

-- | Request body to refund a fiat payment.
data RefundPaymentRequestBody = RefundPaymentRequestBody
  { refundPaymentIdempotencyKey :: !UUID,
    refundPaymentAmount :: !MoneyAmount,
    refundPaymentReason :: !(Maybe CancelPaymentReason)
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec RefundPaymentRequestBody)

instance HasCodec RefundPaymentRequestBody where
  codec =
    object "RefundPaymentRequestBody" $
      RefundPaymentRequestBody
        <$> requiredField' "idempotencyKey" .= refundPaymentIdempotencyKey
        <*> requiredField' "amount" .= refundPaymentAmount
        <*> optionalField' "reason" .= refundPaymentReason

---------------------------------------------------------------
-- On-chain Payment Types
---------------------------------------------------------------

data OnChainTransferRequest

type instance CircleRequest OnChainTransferRequest = CircleResponseBody TransferResponseBody

instance CircleHasParam OnChainTransferRequest ReturnIdentitiesQueryParam

data OnChainTransfersRequest

type instance CircleRequest OnChainTransfersRequest = CircleResponseBody [TransferResponseBody]

instance CircleHasParam OnChainTransfersRequest PaginationQueryParams

instance CircleHasParam OnChainTransfersRequest FromQueryParam

instance CircleHasParam OnChainTransfersRequest ToQueryParam

instance CircleHasParam OnChainTransfersRequest PageSizeQueryParam

instance CircleHasParam OnChainTransfersRequest WalletIdQueryParam

instance CircleHasParam OnChainTransfersRequest SourceWalletIdQueryParam

instance CircleHasParam OnChainTransfersRequest DestinationWalletIdQueryParam

instance CircleHasParam OnChainTransfersRequest ReturnIdentitiesQueryParam

data OnChainAddressRequest

type instance CircleRequest OnChainAddressRequest = CircleResponseBody DepositAddressResponseBody

-- | Request body to create an on-chain transfer
data OnChainTransferRequestBody = OnChainTransferRequestBody
  { onChainTransferRequestBodyIdempotencyKey :: !UUID,
    onChainTransferRequestBodySource :: !SourceWallet,
    onChainTransferRequestBodyDestination :: !(ThisOrThat DestinationWallet DestinationBlockchain),
    onChainTransferRequestBodyAmount :: !MoneyAmount
  }
  deriving (Eq, Show)

instance ToJSON OnChainTransferRequestBody where
  toJSON :: OnChainTransferRequestBody -> Aeson.Value
  toJSON OnChainTransferRequestBody {..} =
    Aeson.object
      [ "idempotencyKey" Aeson..= onChainTransferRequestBodyIdempotencyKey,
        "source" Aeson..= onChainTransferRequestBodySource,
        "destination" Aeson..= onChainTransferRequestBodyDestination,
        "amount" Aeson..= onChainTransferRequestBodyAmount
      ]

---------------------------------------------------------------
-- Card Types
---------------------------------------------------------------

data CardsRequest

type instance CircleRequest CardsRequest = CircleResponseBody [ListCardResponseBody]

instance CircleHasParam CardsRequest PaginationQueryParams

instance CircleHasParam CardsRequest PageSizeQueryParam

data CardRequest

type instance CircleRequest CardRequest = CircleResponseBody CardResponseBody

data ListCardResponseBody = ListCardResponseBody
  { listCardId :: !UUID,
    listCardStatus :: !Status,
    listCardBillingDetails :: !ListCardBillingDetails,
    listCardExpiryMonth :: !Int,
    listCardExpiryYear :: !Int,
    listCardNetwork :: !CardNetwork,
    listCardBin :: !(Maybe Text), -- first 6 digits of the Card, should be a custom newtype
    listCardIssuerCountry :: !(Maybe ISO3166Alpha2),
    listCardFingerprint :: !UUID,
    listCardVerification :: !VerificationData,
    listCardRiskEvaluation :: !(Maybe RiskEvaluation),
    listCardCreateDate :: !UTCTime,
    listCardUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ListCardResponseBody)

instance HasCodec ListCardResponseBody where
  codec =
    object "ListCardResponseBody" $
      ListCardResponseBody
        <$> requiredField' "id" .= listCardId
        <*> requiredField' "status" .= listCardStatus
        <*> requiredField' "billingDetails" .= listCardBillingDetails
        <*> requiredField' "expMonth" .= listCardExpiryMonth
        <*> requiredField' "expYear" .= listCardExpiryYear
        <*> requiredField' "network" .= listCardNetwork
        <*> optionalField' "bin" .= listCardBin
        <*> optionalField' "issuerCountry" .= listCardIssuerCountry
        <*> requiredField' "fingerprint" .= listCardFingerprint
        <*> requiredField' "verification" .= listCardVerification
        <*> optionalField' "riskEvaluation" .= listCardRiskEvaluation
        <*> requiredField' "createDate" .= listCardCreateDate
        <*> requiredField' "updateDate" .= listCardUpdateDate

data CardResponseBody = CardResponseBody
  { cardId :: !UUID,
    cardStatus :: !Status,
    cardBillingDetails :: !BillingDetails,
    cardExpiryMonth :: !Int,
    cardExpiryYear :: !Int,
    cardNetwork :: !CardNetwork,
    cardLast4 :: !Text, -- last 4 digits of card, should be a custom type
    cardBin :: !(Maybe Text), -- first 6 digits of the card, should be a custom newtype
    cardIssuerCountry :: !(Maybe ISO3166Alpha2),
    cardFundingType :: !(Maybe CardFundingType),
    cardFingerprint :: !UUID,
    cardErrorCode :: !(Maybe VerificationErrorCode),
    cardVerification :: !VerificationData,
    cardRiskEvaluation :: !(Maybe RiskEvaluation),
    cardMetadata :: !ResponseMetadata,
    cardCreateDate :: !UTCTime,
    cardUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CardResponseBody)

instance HasCodec CardResponseBody where
  codec =
    object "CardResponseBody" $
      CardResponseBody
        <$> requiredField' "id" .= cardId
        <*> requiredField' "status" .= cardStatus
        <*> requiredField' "billingDetails" .= cardBillingDetails
        <*> requiredField' "expMonth" .= cardExpiryMonth
        <*> requiredField' "expYear" .= cardExpiryYear
        <*> requiredField' "network" .= cardNetwork
        <*> requiredField' "last4" .= cardLast4
        <*> optionalField' "bin" .= cardBin
        <*> optionalField' "issuerCountry" .= cardIssuerCountry
        <*> optionalField' "fundingType" .= cardFundingType
        <*> requiredField' "fingerprint" .= cardFingerprint
        <*> optionalField' "errorCode" .= cardErrorCode
        <*> requiredField' "verification" .= cardVerification
        <*> optionalField' "riskEvaluation" .= cardRiskEvaluation
        <*> requiredField' "metadata" .= cardMetadata
        <*> requiredField' "createDate" .= cardCreateDate
        <*> requiredField' "updateDate" .= cardUpdateDate

-- | Request body to create a debit card.
data CreateCardRequestBody = CreateCardRequestBody
  { createCardIdempotencyKey :: !UUID,
    createCardKeyId :: !(Maybe Text), -- key1 in sandbox
    createCardEncryptedData :: !(Maybe Text), -- NB: this encrypted data contains the CVV AND the card number somehow
    createCardBillingDetails :: !BillingDetails,
    createCardExpiryMonth :: !Int,
    createCardExpiryYear :: !Int,
    createCardMetadata :: !RequestMetadata
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreateCardRequestBody)

instance HasCodec CreateCardRequestBody where
  codec =
    object "CreateCardRequestBody" $
      CreateCardRequestBody
        <$> requiredField' "idempotencyKey" .= createCardIdempotencyKey
        <*> optionalField' "keyId" .= createCardKeyId
        <*> requiredField' "encryptedData" .= createCardEncryptedData
        <*> requiredField' "billingDetails" .= createCardBillingDetails
        <*> requiredField' "expMonth" .= createCardExpiryMonth
        <*> requiredField' "expYear" .= createCardExpiryYear
        <*> requiredField' "metadata" .= createCardMetadata

-- | Request body to update a debit card.
data UpdateCardRequestBody = UpdateCardRequestBody
  { updateCardKeyId :: !(Maybe Text), -- key1 in sandbox
    updateCardEncryptedData :: !(Maybe Text), -- NB: this encrypted data contains the CVV AND the card number somehow
    updateCardExpiryMonth :: !Int,
    updateCardExpiryYear :: !Int
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec UpdateCardRequestBody)

instance HasCodec UpdateCardRequestBody where
  codec =
    object "UpdateCardRequestBody" $
      UpdateCardRequestBody
        <$> requiredField' "keyId" .= updateCardKeyId
        <*> requiredField' "encryptedData" .= updateCardEncryptedData
        <*> requiredField' "expMonth" .= updateCardExpiryMonth
        <*> requiredField' "expYear" .= updateCardExpiryYear

data ListCardBillingDetails = ListCardBillingDetails
  { listCardBillingDetailsCountry :: !ISO3166Alpha2,
    listCardBillingDetailsDistrict :: !District
  }
  deriving (Eq, Show, Generic)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec ListCardBillingDetails)

instance HasCodec ListCardBillingDetails where
  codec =
    object "ListCardBillingDetails" $
      ListCardBillingDetails
        <$> requiredField' "country" .= listCardBillingDetailsCountry
        <*> requiredField' "district" .= listCardBillingDetailsDistrict

data CardNetwork
  = VISA
  | MASTERCARD
  | AMEX
  | UNKNOWN
  deriving (Eq, Show, Enum, Bounded)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CardNetwork)

instance HasCodec CardNetwork where
  codec = shownBoundedEnumCodec

data CardFundingType
  = Credit
  | Debit
  | Prepaid
  | Unknown
  deriving (Eq, Show, Enum, Bounded)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CardFundingType)

instance HasCodec CardFundingType where
  codec =
    stringConstCodec $
      NE.fromList
        [ (Credit, "credit"),
          (Debit, "debit"),
          (Prepaid, "prepaid"),
          (Unknown, "unknown")
        ]

data VerificationErrorCode
  = VerificationFailed
  | VerificationFraudDetected
  | VerificationDenied
  | VerificationNotSupportedByIssuer
  | VerificationStoppedByIssuer
  | VerificationCardFailed
  | VerificationCardInvalid
  | VerificationCardAddressMismatch
  | VerificationCardZipMismatch
  | VerificationCardCvvInvalid
  | VerificationCardExpired
  | VerificationCardLimitViolated
  | VerificationCardNotHonored
  | VerificationCardCvvRequired
  | VerificationCreditCardNotAllowed
  | VerificationCardAccountIneligible
  | VerificationCardNetworkUnsupported
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec VerificationErrorCode)

instance HasCodec VerificationErrorCode where
  codec =
    stringConstCodec $
      NE.fromList
        [ (VerificationFailed, "verification_failed"),
          (VerificationFraudDetected, "verification_fraud_detected"),
          (VerificationDenied, "verification_denied"),
          (VerificationNotSupportedByIssuer, "verification_not_supported_by_issuer"),
          (VerificationStoppedByIssuer, "verification_stopped_by_issuer"),
          (VerificationCardFailed, "card_failed"),
          (VerificationCardInvalid, "card_invalid"),
          (VerificationCardAddressMismatch, "card_address_mismatch"),
          (VerificationCardZipMismatch, "card_zip_mismatch"),
          (VerificationCardCvvInvalid, "card_cvv_invalid"),
          (VerificationCardExpired, "card_expired"),
          (VerificationCardLimitViolated, "card_limit_violated"),
          (VerificationCardNotHonored, "card_not_honored"),
          (VerificationCardCvvRequired, "card_cvv_required"),
          (VerificationCreditCardNotAllowed, "credit_card_not_allowed"),
          (VerificationCardAccountIneligible, "card_account_ineligible"),
          (VerificationCardNetworkUnsupported, "card_network_unsupported")
        ]

---------------------------------------------------------------
-- ACH Types
---------------------------------------------------------------

data ACHBankAccountRequest

type instance CircleRequest ACHBankAccountRequest = CircleResponseBody ACHBankAccountResponseBody

data ACHBankAccountResponseBody = ACHBankAccountResponseBody
  { achBankAccountId :: !UUID,
    achBankAccountStatus :: !Status,
    achBankAccountAccountNumber :: !AccountNumber,
    achBankAccountRoutingNumber :: !RoutingNumber,
    achBankAccountBillingDetails :: !BillingDetails,
    achBankAccountType :: !(Maybe ACHBankAccountType),
    achBankAccountBankAddress :: !BankAddress,
    achBankAccountFingerprint :: !UUID,
    achBankAccountErrorCode :: !(Maybe ACHBankAccountErrorCode),
    achBankAccountRiskEvaluation :: !(Maybe RiskEvaluation),
    achBankAccountMetadata :: !ResponseMetadata,
    achBankAccountCreateDate :: !UTCTime,
    achBankAccountUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ACHBankAccountResponseBody)

instance HasCodec ACHBankAccountResponseBody where
  codec =
    object "ACHBankAccountResponseBody" $
      ACHBankAccountResponseBody
        <$> requiredField' "id" .= achBankAccountId
        <*> requiredField' "status" .= achBankAccountStatus
        <*> requiredField' "accountNumber" .= achBankAccountAccountNumber
        <*> requiredField' "routingNumber" .= achBankAccountRoutingNumber
        <*> requiredField' "billingDetails" .= achBankAccountBillingDetails
        <*> optionalField' "bankAccountType" .= achBankAccountType
        <*> requiredField' "bankAddress" .= achBankAccountBankAddress
        <*> requiredField' "fingerprint" .= achBankAccountFingerprint
        <*> optionalField' "errorCode" .= achBankAccountErrorCode
        <*> optionalField' "riskEvaluation" .= achBankAccountRiskEvaluation
        <*> requiredField' "metadata" .= achBankAccountMetadata
        <*> requiredField' "createDate" .= achBankAccountCreateDate
        <*> requiredField' "updateDate" .= achBankAccountUpdateDate

data ACHBankAccountErrorCode
  = ACHBankAccountAuthorizationExpired
  | ACHBankAccountError
  | ACHBankAccountIneligible
  | ACHBankAccountNotFound
  | ACHBankAccountUnauthorized
  | ACHBankAccountUnsupportedRoutingNumber
  | ACHBankAccountVerificationFailed
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ACHBankAccountErrorCode)

instance HasCodec ACHBankAccountErrorCode where
  codec =
    stringConstCodec $
      NE.fromList
        [ (ACHBankAccountAuthorizationExpired, "bank_account_authorization_expired"),
          (ACHBankAccountError, "bank_account_error"),
          (ACHBankAccountIneligible, "bank_account_ineligible"),
          (ACHBankAccountNotFound, "bank_account_not_found"),
          (ACHBankAccountUnauthorized, "bank_account_unauthorized"),
          (ACHBankAccountUnsupportedRoutingNumber, "unsupported_routing_number"),
          (ACHBankAccountVerificationFailed, "verification_failed")
        ]

-- | Request body to an ACH bank account.
data CreateACHBankAccountRequestBody = CreateACHBankAccountRequestBody
  { achBankAccountBodyIdempotencyKey :: !UUID,
    achBankAccountBodyPlaidProcessorToken :: !ProcessorToken,
    achBankAccountBodyBillingDetails :: !BillingDetails,
    achBankAccountBodyBankAccountType :: !(Maybe ACHBankAccountType),
    achBankAccountBodyMetadata :: !RequestMetadata
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreateACHBankAccountRequestBody)

instance HasCodec CreateACHBankAccountRequestBody where
  codec =
    object "CreateACHBankAccountRequestBody" $
      CreateACHBankAccountRequestBody
        <$> requiredField' "idempotencyKey" .= achBankAccountBodyIdempotencyKey
        <*> requiredField' "plaidProcessorToken" .= achBankAccountBodyPlaidProcessorToken
        <*> requiredField' "billingDetails" .= achBankAccountBodyBillingDetails
        <*> optionalField' "bankAccountType" .= achBankAccountBodyBankAccountType
        <*> requiredField' "metadata" .= achBankAccountBodyMetadata

---------------------------------------------------------------
-- Mock Account Types
---------------------------------------------------------------

data MockAccountRequest

type instance CircleRequest MockAccountRequest = CircleResponseBody MockACHBankAccountResponseBody

-- | Request body to create a mock ACH bank account (in the sandbox only).
data CreateMockACHBankAccountRequestBody = CreateMockACHBankAccountRequestBody
  { mockACHBankAccountBodyAccount :: !MockACHBankAccount,
    mockACHBankAccountBodyBalance :: !MoneyAmount
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreateMockACHBankAccountRequestBody)

instance HasCodec CreateMockACHBankAccountRequestBody where
  codec =
    object "CreateMockACHBankAccountRequestBody" $
      CreateMockACHBankAccountRequestBody
        <$> requiredField' "account" .= mockACHBankAccountBodyAccount
        <*> requiredField' "balance" .= mockACHBankAccountBodyBalance

data MockACHBankAccountResponseBody = MockACHBankAccountResponseBody
  { mockACHBankAccountResponseBodyAccount :: !MockACHBankAccount,
    mockACHBankAccountResponseBodyBalance :: !MoneyAmount,
    -- in sandbox, the value of this token is processor-sandbox-circle-82cf95bb-43f8-4191-8d30-2c9f42853621
    mockACHBankAccountResponseBodyProcessorToken :: !ProcessorToken
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MockACHBankAccountResponseBody)

instance HasCodec MockACHBankAccountResponseBody where
  codec =
    object "MockACHBankAccountResponseBody" $
      MockACHBankAccountResponseBody
        <$> requiredField' "account" .= mockACHBankAccountResponseBodyAccount
        <*> requiredField' "balance" .= mockACHBankAccountResponseBodyBalance
        <*> requiredField' "processorToken" .= mockACHBankAccountResponseBodyProcessorToken

data MockACHBankAccount = MockACHBankAccount
  { mockACHBankAccountAccountNumber :: !AccountNumber,
    mockACHBankAccountRoutingNumber :: !MockRoutingNumber,
    mockACHBankAccountDescription :: !Text
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MockACHBankAccount)

instance HasCodec MockACHBankAccount where
  codec =
    object "MockACHBankAccount" $
      MockACHBankAccount
        <$> requiredField' "accountNumber" .= mockACHBankAccountAccountNumber
        <*> requiredField' "routingNumber" .= mockACHBankAccountRoutingNumber
        <*> requiredField' "description" .= mockACHBankAccountDescription

data MockRoutingNumber
  = MockRoutingNumber1
  | MockRoutingNumber2
  | MockRoutingNumber3
  | MockRoutingNumber4
  | MockRoutingNumber5
  | MockRoutingNumber6
  | MockRoutingNumber7
  | MockRoutingNumber8
  | MockRoutingNumber9
  deriving (Eq, Show, Enum, Bounded)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MockRoutingNumber)

instance HasCodec MockRoutingNumber where
  codec =
    stringConstCodec $
      NE.fromList
        [ (MockRoutingNumber1, "011000028"),
          (MockRoutingNumber2, "011201762"),
          (MockRoutingNumber3, "011500120"),
          (MockRoutingNumber4, "021214862"),
          (MockRoutingNumber5, "121000248"),
          (MockRoutingNumber6, "121140399"),
          (MockRoutingNumber7, "211073473"),
          (MockRoutingNumber8, "221172610"),
          (MockRoutingNumber9, "011000138")
        ]

---------------------------------------------------------------
-- SEPA Types
---------------------------------------------------------------

data SEPAAccountRequest

type instance CircleRequest SEPAAccountRequest = CircleResponseBody SEPAAccountResponseBody

data SEPAInstructionsRequest

type instance CircleRequest SEPAInstructionsRequest = CircleResponseBody WireInstructionsResponseData

-- | Request body to create a SEPA account.
data SEPAAccountRequestBody = SEPAAccountRequestBody
  { sepaAccountRequestBodyIdempotencyKey :: !UUID,
    sepaAccountRequestBodyIBAN :: !Iban,
    sepaAccountRequestBodyBillingDetails :: !BillingDetails
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SEPAAccountRequestBody)

instance HasCodec SEPAAccountRequestBody where
  codec =
    object "SEPAAccountRequestBody" $
      SEPAAccountRequestBody
        <$> requiredField' "idempotencyKey" .= sepaAccountRequestBodyIdempotencyKey
        <*> requiredField' "iban" .= sepaAccountRequestBodyIBAN
        <*> requiredField' "billingDetails" .= sepaAccountRequestBodyBillingDetails

data SEPAAccountResponseBody = SEPAAccountResponseBody
  { sepaAccountResponseBodyId :: !UUID,
    sepaAccountResponseBodyStatus :: !Status,
    sepaAccountResponseBodyDescription :: !Text, -- TODO better type: Bank name plus last four digits of the bank account number or IBAN.  Make a custom type for this
    sepaAccountResponseBodyTrackingRef :: !TrackingReference,
    sepaAccountResponseBodyFingerprint :: !UUID,
    sepaAccountResponseBodyRiskEvaluation :: !(Maybe RiskEvaluation),
    sepaAccountResponseBodyBillingDetails :: !BillingDetails,
    sepaAccountResponseBodyCreateDate :: !UTCTime,
    sepaAccountResponseBodyUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SEPAAccountResponseBody)

instance HasCodec SEPAAccountResponseBody where
  codec =
    object "SEPAAccountResponseBody" $
      SEPAAccountResponseBody
        <$> requiredField' "id" .= sepaAccountResponseBodyId
        <*> requiredField' "status" .= sepaAccountResponseBodyStatus
        <*> requiredField' "description" .= sepaAccountResponseBodyDescription
        <*> requiredField' "trackingRef" .= sepaAccountResponseBodyTrackingRef
        <*> requiredField' "fingerprint" .= sepaAccountResponseBodyFingerprint
        <*> optionalField' "riskEvaluation" .= sepaAccountResponseBodyRiskEvaluation
        <*> requiredField' "billingDetails" .= sepaAccountResponseBodyBillingDetails
        <*> requiredField' "createDate" .= sepaAccountResponseBodyCreateDate
        <*> requiredField' "updateDate" .= sepaAccountResponseBodyUpdateDate

---------------------------------------------------------------
-- Settlements Types
---------------------------------------------------------------

data SettlementRequest

type instance CircleRequest SettlementRequest = CircleResponseBody SettlementResponseBody

data SettlementsRequest

type instance CircleRequest SettlementsRequest = CircleResponseBody [SettlementResponseBody]

instance CircleHasParam SettlementsRequest PaginationQueryParams

instance CircleHasParam SettlementsRequest FromQueryParam

instance CircleHasParam SettlementsRequest ToQueryParam

instance CircleHasParam SettlementsRequest PageSizeQueryParam

data SettlementResponseBody = SettlementResponseBody
  { settlementResponseBodyId :: !UUID,
    settlementResponseBodyMerchantWalletId :: !UUID,
    settlementResponseBodyWalletId :: !UUID,
    settlementResponseBodyTotalDebits :: !MoneyAmount,
    settlementResponseBodyTotalCredits :: !MoneyAmount,
    settlementResponseBodyPaymentFees :: !MoneyAmount,
    settlementResponseBodyChargebackFees :: !MoneyAmount,
    settlementResponseBodyCreateDate :: !UTCTime,
    settlementResponseBodyUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SettlementResponseBody)

instance HasCodec SettlementResponseBody where
  codec =
    object "SettlementResponseBody" $
      SettlementResponseBody
        <$> requiredField' "id" .= settlementResponseBodyId
        <*> requiredField' "merchantWalletId" .= settlementResponseBodyMerchantWalletId
        <*> requiredField' "walletId" .= settlementResponseBodyWalletId
        <*> requiredField' "totalDebits" .= settlementResponseBodyTotalDebits
        <*> requiredField' "totalCredits" .= settlementResponseBodyTotalCredits
        <*> requiredField' "paymentFees" .= settlementResponseBodyPaymentFees
        <*> requiredField' "chargebackFees" .= settlementResponseBodyChargebackFees
        <*> requiredField' "createDate" .= settlementResponseBodyCreateDate
        <*> requiredField' "updateDate" .= settlementResponseBodyUpdateDate

---------------------------------------------------------------
-- Chargeback Types
---------------------------------------------------------------

data ChargebacksRequest

type instance CircleRequest ChargebacksRequest = CircleResponseBody [ChargebackResponseBody]

instance CircleHasParam ChargebacksRequest PaginationQueryParams

instance CircleHasParam ChargebacksRequest FromQueryParam

instance CircleHasParam ChargebacksRequest ToQueryParam

instance CircleHasParam ChargebacksRequest PageSizeQueryParam

instance CircleHasParam ChargebacksRequest PaymentIdQueryParam

data ChargebackRequest

type instance CircleRequest ChargebackRequest = CircleResponseBody ChargebackResponseBody

data MockChargebackRequest

type instance CircleRequest MockChargebackRequest = CircleResponseBody ChargebackResponseBody

data ChargebackResponseBody = ChargebackResponseBody
  { chargebackResponseBodyId :: !UUID,
    chargebackResponseBodyPaymentId :: !UUID,
    chargebackResponseBodyMerchantId :: !UUID,
    chargebackResponseBodyReasonCode :: !Text, -- it's open-ended, no type will save this
    chargebackResponseBodyCategory :: !(Maybe ChargebackCategory),
    chargebackResponseBodyHistory :: [ChargebackHistory]
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ChargebackResponseBody)

instance HasCodec ChargebackResponseBody where
  codec =
    object "ChargebackResponseBody" $
      ChargebackResponseBody
        <$> requiredField' "id" .= chargebackResponseBodyId
        <*> requiredField' "paymentId" .= chargebackResponseBodyPaymentId
        <*> requiredField' "merchantId" .= chargebackResponseBodyMerchantId
        <*> requiredField' "reasonCode" .= chargebackResponseBodyReasonCode
        <*> optionalField' "category" .= chargebackResponseBodyCategory
        <*> requiredField' "history" .= chargebackResponseBodyHistory

data ChargebackCategory
  = CanceledRecurringPayment
  | CustomerDispute
  | Fraudulent
  | General
  | ProcessingError
  | NotDefined
  deriving (Eq, Show, Enum, Bounded)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ChargebackCategory)

instance HasCodec ChargebackCategory where
  codec =
    stringConstCodec $
      NE.fromList
        [ (CanceledRecurringPayment, "Canceled Recurring Payment"),
          (CustomerDispute, "Customer Dispute"),
          (Fraudulent, "Fraudulent"),
          (General, "General"),
          (ProcessingError, "Processing Error"),
          (NotDefined, "Not Defined")
        ]

data ChargebackHistory = ChargebackHistory
  { chargebackHistoryType :: !ChargebackHistoryType,
    chargebackHistoryAmount :: !MoneyAmount,
    chargebackHistoryFee :: !(Maybe MoneyAmount),
    chargebackHistoryDescription :: !Text,
    chargebackHistorySettlementId :: !(Maybe UUID),
    chargebackHistoryCreateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ChargebackHistory)

instance HasCodec ChargebackHistory where
  codec =
    object "ChargebackHistory" $
      ChargebackHistory
        <$> requiredField' "type" .= chargebackHistoryType
        <*> requiredField' "amount" .= chargebackHistoryAmount
        <*> requiredField' "fee" .= chargebackHistoryFee
        <*> requiredField' "description" .= chargebackHistoryDescription
        <*> optionalField' "settlementId" .= chargebackHistorySettlementId
        <*> requiredField' "createDate" .= chargebackHistoryCreateDate

data ChargebackHistoryType
  = FirstChargeback
  | SecondChargeback
  | ChargebackReversal
  | Representment
  | ChargebackSettlement
  deriving (Eq, Show, Enum, Bounded)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ChargebackHistoryType)

instance HasCodec ChargebackHistoryType where
  codec =
    stringConstCodec $
      NE.fromList
        [ (FirstChargeback, "First Chargeback"),
          (SecondChargeback, "Second Chargeback"),
          (ChargebackReversal, "Chargeback Reversal"),
          (Representment, "Representment"),
          (ChargebackSettlement, "Chargeback Settlement")
        ]

---------------------------------------------------------------
-- Reversal Types
---------------------------------------------------------------

data ReversalsRequest

type instance CircleRequest ReversalsRequest = CircleResponseBody [ReversalResponseBody]

instance CircleHasParam ReversalsRequest PaginationQueryParams

instance CircleHasParam ReversalsRequest FromQueryParam

instance CircleHasParam ReversalsRequest ToQueryParam

instance CircleHasParam ReversalsRequest PageSizeQueryParam

instance CircleHasParam ReversalsRequest PaymentStatusQueryParams

data ReversalResponseBody = ReversalResponseBody
  { reversalResponseBodyId :: !UUID,
    reversalResponseBodyPaymentId :: !UUID,
    reversalResponseBodyAmount :: !MoneyAmount,
    reversalResponseBodyDescription :: !Text,
    reversalResponseBodyStatus :: !Status,
    reversalResponseBodyReason :: !ReversalReason,
    reversalResponseBodyFees :: !MoneyAmount,
    reversalResponseBodyCreateDate :: !UTCTime,
    reversalResponseBodyUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ReversalResponseBody)

instance HasCodec ReversalResponseBody where
  codec =
    object "ReversalResponseBody" $
      ReversalResponseBody
        <$> requiredField' "id" .= reversalResponseBodyId
        <*> requiredField' "paymentId" .= reversalResponseBodyPaymentId
        <*> requiredField' "amount" .= reversalResponseBodyAmount
        <*> requiredField' "description" .= reversalResponseBodyDescription
        <*> requiredField' "status" .= reversalResponseBodyStatus
        <*> requiredField' "reason" .= reversalResponseBodyReason
        <*> requiredField' "fees" .= reversalResponseBodyFees
        <*> requiredField' "createDate" .= reversalResponseBodyCreateDate
        <*> requiredField' "updateDate" .= reversalResponseBodyUpdateDate

data ReversalReason
  = ReversalDuplicate
  | ReversalFraudulent
  | ReversalRequestedByCustomer
  | ReversalBankTransactionError
  | ReversalInvalidAccountNumber
  | ReversalInsufficientFunds
  | ReversalPaymentStoppedByIssuer
  | ReversalPaymentReturned
  | ReversalBankAccountIneligible
  | ReversalInvalidACHRTN
  | ReversalUnauthorizedTransaction
  | ReversalPaymentFailed
  deriving (Eq, Show, Enum, Bounded)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ReversalReason)

instance HasCodec ReversalReason where
  codec =
    stringConstCodec $
      NE.fromList
        [ (ReversalDuplicate, "duplicate"),
          (ReversalFraudulent, "fraudulent"),
          (ReversalRequestedByCustomer, "requested_by_customer"),
          (ReversalBankTransactionError, "bank_transaction_error"),
          (ReversalInvalidAccountNumber, "invalid_account_number"),
          (ReversalInsufficientFunds, "insufficient_funds"),
          (ReversalPaymentStoppedByIssuer, "payment_stopped_by_issuer"),
          (ReversalPaymentReturned, "payment_returned"),
          (ReversalBankAccountIneligible, "bank_account_ineligible"),
          (ReversalInvalidACHRTN, "invalid_ach_rtn"),
          (ReversalUnauthorizedTransaction, "unauthorized_transaction"),
          (ReversalPaymentFailed, "payment_failed")
        ]

---------------------------------------------------------------
-- Payment Intent Types
---------------------------------------------------------------

data PaymentIntentRequest

type instance CircleRequest PaymentIntentRequest = CircleResponseBody PaymentIntentResponseBody

data PaymentIntentsRequest

type instance CircleRequest PaymentIntentsRequest = CircleResponseBody [PaymentIntentResponseBody]

instance CircleHasParam PaymentIntentsRequest PaginationQueryParams

instance CircleHasParam PaymentIntentsRequest FromQueryParam

instance CircleHasParam PaymentIntentsRequest ToQueryParam

instance CircleHasParam PaymentIntentsRequest PageSizeQueryParam

instance CircleHasParam PaymentIntentsRequest PaymentStatusQueryParams

instance CircleHasParam PaymentIntentsRequest PaymentIntentContextQueryParams

-- | Request body to create a payment intent for a blockchain payment
data CreatePaymentIntentRequestBody = CreatePaymentIntentRequestBody
  { createPaymentIntentIdempotencyKey :: !UUID,
    createPaymentIntentAmount :: !MoneyAmount,
    createPaymentIntentSettlementCurrency :: !SupportedCurrencies,
    createPaymentIntentPaymentMethods :: [PaymentMethodData]
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreatePaymentIntentRequestBody)

instance HasCodec CreatePaymentIntentRequestBody where
  codec =
    object "CreatePaymentIntentRequestBody" $
      CreatePaymentIntentRequestBody
        <$> requiredField' "idempotencyKey" .= createPaymentIntentIdempotencyKey
        <*> requiredField' "amount" .= createPaymentIntentAmount
        <*> requiredField' "settlementCurrency" .= createPaymentIntentSettlementCurrency
        <*> requiredField' "paymentMethods" .= createPaymentIntentPaymentMethods

data PaymentIntentResponseBody = PaymentIntentResponseBody
  { paymentIntentResponseBodyIdempotencyKey :: !UUID,
    paymentIntentResponseBodyId :: !UUID,
    paymentIntentResponseBodyAmount :: !MoneyAmount,
    paymentIntentResponseBodyAmountPaid :: !MoneyAmount,
    paymentIntentResponseBodySettlementCurrency :: !SupportedCurrencies,
    paymentIntentResponseBodyPaymentMethods :: ![PaymentMethodData],
    paymentIntentResponseBodyFees :: ![BlockchainFeeMoneyAmount],
    paymentIntentResponseBodyPaymentIds :: ![UUID],
    paymentIntentResponseBodyTimeline :: ![TimelineData],
    paymentIntentResponseBodyExpiresOn :: !UTCTime,
    paymentIntentResponseBodyUpdateDate :: !UTCTime,
    paymentIntentResponseBodyCreateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentIntentResponseBody)

instance HasCodec PaymentIntentResponseBody where
  codec =
    object "PaymentIntentResponseBody" $
      PaymentIntentResponseBody
        <$> requiredField' "idempotencyKey" .= paymentIntentResponseBodyIdempotencyKey
        <*> requiredField' "id" .= paymentIntentResponseBodyId
        <*> requiredField' "amount" .= paymentIntentResponseBodyAmount
        <*> requiredField' "amountPaid" .= paymentIntentResponseBodyAmountPaid
        <*> requiredField' "settlementCurrency" .= paymentIntentResponseBodySettlementCurrency
        <*> requiredField' "paymentMethods" .= paymentIntentResponseBodyPaymentMethods
        <*> requiredField' "fees" .= paymentIntentResponseBodyFees
        <*> requiredField' "paymentIds" .= paymentIntentResponseBodyPaymentIds
        <*> requiredField' "timeline" .= paymentIntentResponseBodyTimeline
        <*> requiredField' "expiresOn" .= paymentIntentResponseBodyExpiresOn
        <*> requiredField' "updateDate" .= paymentIntentResponseBodyUpdateDate
        <*> requiredField' "createDate" .= paymentIntentResponseBodyCreateDate

data PaymentMethodData = PaymentMethodData
  { paymentMethodType :: !Text, -- just "blockchain"
    paymentMethodDataChain :: !Chain,
    paymentMethodDataAddress :: !(Maybe HexString)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentMethodData)

instance HasCodec PaymentMethodData where
  codec =
    object "PaymentMethodData" $
      PaymentMethodData
        <$> requiredField' "type" .= paymentMethodType
        <*> requiredField' "chain" .= paymentMethodDataChain
        <*> optionalField' "address" .= paymentMethodDataAddress

data TimelineData = TimelineData
  { timelineDataStatus :: !PaymentIntentStatus,
    timelineDataContext :: !PaymentIntentContext,
    timelineDataTime :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec TimelineData)

instance HasCodec TimelineData where
  codec =
    object "TimelineData" $
      TimelineData
        <$> requiredField' "status" .= timelineDataStatus
        <*> requiredField' "context" .= timelineDataContext
        <*> requiredField' "time" .= timelineDataTime

---------------------------------------------------------------
-- Payout Return Types
---------------------------------------------------------------

data ReturnsRequest

type instance CircleRequest ReturnsRequest = CircleResponseBody [PayoutReturnResponseBody]

instance CircleHasParam ReturnsRequest PaginationQueryParams

instance CircleHasParam ReturnsRequest FromQueryParam

instance CircleHasParam ReturnsRequest ToQueryParam

instance CircleHasParam ReturnsRequest PageSizeQueryParam

---------------------------------------------------------------
-- Wallet Types
---------------------------------------------------------------

data WalletRequest

type instance CircleRequest WalletRequest = CircleResponseBody WalletResponseBody

data WalletsRequest

type instance CircleRequest WalletsRequest = CircleResponseBody [WalletResponseBody]

instance CircleHasParam WalletsRequest PaginationQueryParams

instance CircleHasParam WalletsRequest FromQueryParam

instance CircleHasParam WalletsRequest ToQueryParam

instance CircleHasParam WalletsRequest PageSizeQueryParam

data WalletResponseBody = WalletResponseBody
  { walletResponseBodyWalletId :: !WalletId,
    walletResponseBodyEntityId :: !UUID,
    walletResponseBodyType :: !Text, -- This value will always be "end_user_wallet"
    walletResponseBodyDescription :: !(Maybe Text),
    walletResponseBodyBalances :: [MoneyAmount]
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec WalletResponseBody)

instance HasCodec WalletResponseBody where
  codec =
    object "WalletResponseBody" $
      WalletResponseBody
        <$> requiredField' "walletId" .= walletResponseBodyWalletId
        <*> requiredField' "entityId" .= walletResponseBodyEntityId
        <*> requiredField' "type" .= walletResponseBodyType
        <*> optionalField' "description" .= walletResponseBodyDescription
        <*> requiredField' "balances" .= walletResponseBodyBalances

-- | Request body to create a Circle wallet.
data CreateWalletRequestBody = CreateWalletRequestBody
  { createWalletRequestBodyIdempotencyKey :: !UUID,
    createWalletRequestBodyDescription :: !(Maybe Text)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreateWalletRequestBody)

instance HasCodec CreateWalletRequestBody where
  codec =
    object "CreateWalletRequestBody" $
      CreateWalletRequestBody
        <$> requiredField' "idempotencyKey" .= createWalletRequestBodyIdempotencyKey
        <*> optionalField' "description" .= createWalletRequestBodyDescription

---------------------------------------------------------------
-- Utils
---------------------------------------------------------------

utcToCircle :: UTCTime -> Text
utcToCircle ut =
  tshow day <> "T" <> clockTime <> "-00:00"
  where
    day = utctDay ut
    time = utctDayTime ut
    tod = snd $ utcToLocalTimeOfDay utc (timeToTimeOfDay time)
    atLeastTwo :: Text -> Int -> Text
    atLeastTwo t i
      | i < 10 = t <> tshow i
      | otherwise = tshow i
    clockTime =
      atLeastTwo "0" (todHour tod)
        <> ":"
        <> atLeastTwo "0" (todMin tod)
        <> ":"
        <> atLeastTwo "0" (floor $ todSec tod)

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Similar to 'Either' but with different 'ToJSON' and 'FromJSON' instances.
-- 'ToJSON' will serialize the payload without adding any kind of tag.
-- 'FromJSON' will first attempt to parse JSON as the first type parameter,
-- and if that fails will then attempt to parse as the second type parameter.
--
-- NB: The order of type parameters make a huge difference!
-- In @'ThisOrThat' A B@, if @A@ is structurally a subtype of @B@, then the
-- parser can never produce a 'That' result! For example, with types
--
-- @
--   data A = A {foo :: String, bar :: Scientific}
--   data B = B {foo :: String, bar :: Scientific, baz :: Bool}
-- @
--
-- @'FromJSON' ('ThisOrThat' A B)@ can never parse a @B@. Any JSON object
-- that includes properties @foo: <string>@ and @bar: <number>@ will parse
-- as an @A@, whether or not the property @baz: <boolean>@ is present. You
-- can fix this by instead using @'ThisOrThat' B A@.
data ThisOrThat a b = This a | That b
  deriving stock (Eq, Generic)

catThises :: [ThisOrThat a b] -> [a]
catThises lst = lst >>= toThis
  where
    toThis = \case
      This a -> [a]
      _ -> []

catThats :: [ThisOrThat a b] -> [b]
catThats lst = lst >>= toThat
  where
    toThat = \case
      That b -> [b]
      _ -> []

instance (Show a, Show b) => Show (ThisOrThat a b) where
  show = \case
    This a -> show a
    That b -> show b

instance (ToJSON a, ToJSON b) => ToJSON (ThisOrThat a b) where
  toJSON (This a) = toJSON a
  toJSON (That b) = toJSON b
  toEncoding (This a) = toEncoding a
  toEncoding (That b) = toEncoding b

instance (FromJSON a, FromJSON b) => FromJSON (ThisOrThat a b) where
  parseJSON val = do
    let parsedA = fromJSON val
        parsedB = fromJSON val
    case (parsedA, parsedB) of
      (Success a, _) -> pure $ This a
      (_, Success b) -> pure $ That b
      (Error thisError, Error thatError) ->
        fail $
          fold
            [ "Failed when parsing a ThisOrThat from JSON.\n",
              "Error on the This: " <> thisError <> "\n",
              "Error on the That: " <> thatError
            ]

instance Bifunctor ThisOrThat where
  bimap f _ (This a) = This (f a)
  bimap _ g (That b) = That (g b)

thisOrThatToEither :: ThisOrThat a b -> Either a b
thisOrThatToEither = \case
  This a -> Left a
  That b -> Right b

thisOrThat :: (a -> c) -> (b -> c) -> ThisOrThat a b -> c
thisOrThat f g tot = either f g $ thisOrThatToEither tot

---------------------------------------------------------------
-- General, shared types
---------------------------------------------------------------
data Status = Pending | Complete | Failed
  deriving (Show, Eq)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec Status)

instance HasCodec Status where
  codec = stringConstCodec $ NE.fromList [(Pending, "pending"), (Complete, "complete"), (Failed, "failed")]

data PaymentStatus
  = PaymentPending
  | Confirmed
  | Paid
  | PaymentFailed
  | ActionRequired
  deriving (Show, Eq)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentStatus)

instance HasCodec PaymentStatus where
  codec =
    stringConstCodec $
      NE.fromList
        [ (Confirmed, "confirmed"),
          (PaymentPending, "pending"),
          (Paid, "paid"),
          (PaymentFailed, "failed"),
          (ActionRequired, "action_required")
        ]

data PaymentIntentStatus
  = PaymentIntentCreated
  | PaymentIntentPending
  | PaymentIntentComplete
  | PaymentIntentExpired
  | PaymentIntentFailed
  deriving (Show, Eq)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentIntentStatus)

instance HasCodec PaymentIntentStatus where
  codec =
    stringConstCodec $
      NE.fromList
        [ (PaymentIntentCreated, "created"),
          (PaymentIntentPending, "pending"),
          (PaymentIntentComplete, "complete"),
          (PaymentIntentExpired, "expired"),
          (PaymentIntentFailed, "failed")
        ]

data PaymentIntentContext
  = ContextUnderpaid
  | ContextPaid
  | ContextOverpaid
  deriving (Show, Eq)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec PaymentIntentContext)

instance HasCodec PaymentIntentContext where
  codec =
    stringConstCodec $
      NE.fromList
        [ (ContextUnderpaid, "underpaid"),
          (ContextPaid, "paid"),
          (ContextOverpaid, "overpaid")
        ]

data Address = Address
  { addressCity :: !(Maybe City),
    addressCountry :: !(Maybe ISO3166Alpha2),
    addressLine1 :: !(Maybe AddressLine),
    addressLine2 :: !(Maybe AddressLine),
    addressDistrict :: !(Maybe District)
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec Address)

instance HasCodec Address where
  codec =
    object "Address" $
      Address
        <$> optionalField' "city" .= addressCity
        <*> optionalField' "country" .= addressCountry
        <*> optionalField' "line1" .= addressLine1
        <*> optionalField' "line2" .= addressLine2
        <*> optionalField' "district" .= addressDistrict

data BankAccountType = Wire | Sen
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec BankAccountType)

instance HasCodec BankAccountType where
  codec = stringConstCodec $ NE.fromList [(Wire, "wire"), (Sen, "sen")]

data ACHBankAccountType = RetailType | BusinessType
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ACHBankAccountType)

instance HasCodec ACHBankAccountType where
  codec = stringConstCodec $ NE.fromList [(RetailType, "retail"), (BusinessType, "business")]

data DestinationBankAccount = DestinationBankAccount
  { destinationBankAccountType :: !BankAccountType,
    destinationBankAccountId :: !UUID,
    destinationBankAccountName :: !(Maybe Text)
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec DestinationBankAccount)

instance HasCodec DestinationBankAccount where
  codec =
    object "DestinationBankAccount" $
      DestinationBankAccount
        <$> requiredField' "type" .= destinationBankAccountType
        <*> requiredField' "id" .= destinationBankAccountId
        <*> optionalField' "name" .= destinationBankAccountName

-- TODO can we do type narrowing to have other types that represent subsets of
-- this one without have to write custom constructors?
data SupportedCurrencies = USD | EUR | BTC | ETH
  deriving (Eq, Show, Enum, Bounded)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SupportedCurrencies)

instance HasCodec SupportedCurrencies where
  codec = shownBoundedEnumCodec

newtype Amount = Amount
  -- TODO consider making this a numeric type, maybe?
  { unAmount :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec Amount where
  codec = dimapCodec Amount unAmount codec

data BlockchainFeeType = BlockchainLeaseFee | TotalPaymentFees
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec BlockchainFeeType)

instance HasCodec BlockchainFeeType where
  codec = stringConstCodec $ NE.fromList [(BlockchainLeaseFee, "blockChainLeaseFee"), (TotalPaymentFees, "totalPaymentFees")]

data BlockchainFeeMoneyAmount = BlockchainFeeMoneyAmount
  { blockchainFeeMoneyAmountType :: !BlockchainFeeType,
    blockchainFeeMoneyAmountAmount :: !Amount,
    blockchainFeeMoneyAmountCurrency :: !SupportedCurrencies
  }
  deriving (Eq, Show, Generic)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec BlockchainFeeMoneyAmount)

instance HasCodec BlockchainFeeMoneyAmount where
  codec =
    object "BlockchainFeeMoneyAmount" $
      BlockchainFeeMoneyAmount
        <$> requiredField' "type" .= blockchainFeeMoneyAmountType
        <*> requiredField' "amount" .= blockchainFeeMoneyAmountAmount
        <*> requiredField' "currency" .= blockchainFeeMoneyAmountCurrency

data MoneyAmount = MoneyAmount
  { moneyAmountAmount :: !Amount,
    moneyAmountCurrency :: !SupportedCurrencies
  }
  deriving (Eq, Show, Generic)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec MoneyAmount)

instance HasCodec MoneyAmount where
  codec =
    object "MoneyAmount" $
      MoneyAmount
        <$> requiredField' "amount" .= moneyAmountAmount
        <*> requiredField' "currency" .= moneyAmountCurrency

data TransferFeeAmount = TransferFeeAmount
  { transferFeeAmountAmount :: !Amount,
    transferFeeAmountCurrency :: !SupportedCurrencies,
    transferFeeAmountType :: !Text
  }
  deriving (Eq, Show, Generic)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec TransferFeeAmount)

instance HasCodec TransferFeeAmount where
  codec =
    object "TransferFeeAmount" $
      TransferFeeAmount
        <$> requiredField' "amount" .= transferFeeAmountAmount
        <*> requiredField' "currency" .= transferFeeAmountCurrency
        <*> requiredField' "type" .= transferFeeAmountType

data Decision = Approved | Denied | Review
  deriving (Eq, Show, Generic)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec Decision)

instance HasCodec Decision where
  codec = stringConstCodec $ NE.fromList [(Approved, "approved"), (Denied, "denied"), (Review, "review")]

data RiskEvaluation = RiskEvaluation
  { riskEvaluationDecision :: !Decision,
    -- Parsing riskEvaluationReason as a text because it's pretty open-ended: https://developers.circle.com/developer/docs/cards-banks-and-payments-risk-evaluation
    -- TODO maybe eventually add type constraints around these potential fields (unlikely though, there are a lot)
    riskEvaluationReason :: !Text
  }
  deriving (Eq, Show, Generic)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec RiskEvaluation)

instance HasCodec RiskEvaluation where
  codec =
    object "RiskEvaluation" $
      RiskEvaluation
        <$> requiredField' "decision" .= riskEvaluationDecision
        <*> requiredField' "reason" .= riskEvaluationReason

data Adjustments = Adjustments
  { adjustmentsFXCredit :: !MoneyAmount,
    adjustmentsFXDebit :: !MoneyAmount
  }
  deriving (Eq, Show, Generic)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec Adjustments)

instance HasCodec Adjustments where
  codec =
    object "Adjustments" $
      Adjustments
        <$> requiredField' "fxCredit" .= adjustmentsFXCredit
        <*> requiredField' "fxDebit" .= adjustmentsFXDebit

data BillingDetails = BillingDetails
  { billingDetailsName :: !Text,
    billingDetailsCity :: !City,
    billingDetailsCountry :: !ISO3166Alpha2,
    billingDetailsLine1 :: !AddressLine, -- address type
    billingDetailsLine2 :: !(Maybe AddressLine), -- secondary address type
    billingDetailsDistrict :: !(Maybe District), -- could be a state type
    billingDetailsPostalCode :: !PostalCode -- postal code type
  }
  deriving (Eq, Show, Generic)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec BillingDetails)

instance HasCodec BillingDetails where
  codec =
    object "BillingDetails" $
      BillingDetails
        <$> requiredField' "name" .= billingDetailsName
        <*> requiredField' "city" .= billingDetailsCity
        <*> requiredField' "country" .= billingDetailsCountry
        <*> requiredField' "line1" .= billingDetailsLine1
        <*> optionalField' "line2" .= billingDetailsLine2
        <*> optionalField' "district" .= billingDetailsDistrict
        <*> requiredField' "postalCode" .= billingDetailsPostalCode

-- TODO refactor this type to use address WithName.  Look into the same stuff that we used at Mercury for WithName
data BankAddress = BankAddress
  { bankAddressName :: !(Maybe Text),
    bankAddressCity :: !(Maybe City),
    bankAddressCountry :: !(Maybe ISO3166Alpha2),
    bankAddressLine1 :: !(Maybe AddressLine),
    bankAddressLine2 :: !(Maybe AddressLine),
    bankAddressDistrict :: !(Maybe District)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec BankAddress)

instance HasCodec BankAddress where
  codec =
    object "BankAddress" $
      BankAddress
        <$> optionalField' "name" .= bankAddressName
        <*> optionalField' "city" .= bankAddressCity
        <*> optionalField' "country" .= bankAddressCountry
        <*> optionalField' "line1" .= bankAddressLine1
        <*> optionalField' "line2" .= bankAddressLine2
        <*> optionalField' "district" .= bankAddressDistrict

data BeneficiaryDetails = BeneficiaryDetails
  { beneficiaryDetailsName :: !Text,
    beneficiaryDetailsAddress1 :: !(Maybe AddressLine),
    beneficiaryDetailsAddress2 :: !(Maybe AddressLine)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec BeneficiaryDetails)

instance HasCodec BeneficiaryDetails where
  codec =
    object "BeneficiaryDetails" $
      BeneficiaryDetails
        <$> requiredField' "name" .= beneficiaryDetailsName
        <*> optionalField' "address1" .= beneficiaryDetailsAddress1
        <*> optionalField' "address2" .= beneficiaryDetailsAddress2

data BeneficiaryBankDetails = BeneficiaryBankDetails
  { beneficiaryBankDetailsName :: !Text,
    beneficiaryBankDetailsSwiftCode :: !SwiftCode,
    beneficiaryBankDetailsRoutingNumber :: !RoutingNumber,
    beneficiaryBankDetailsAccountNumber :: !AccountNumber,
    beneficiaryBankDetailsCurrency :: !SupportedCurrencies,
    beneficiaryBankDetailsAddress :: !AddressLine,
    beneficiaryBankDetailsCity :: !City,
    beneficiaryBankDetailsPostalCode :: !PostalCode,
    beneficiaryBankDetailsCountry :: !ISO3166Alpha2
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec BeneficiaryBankDetails)

instance HasCodec BeneficiaryBankDetails where
  codec =
    object "BeneficiaryBankDetails" $
      BeneficiaryBankDetails
        <$> requiredField' "name" .= beneficiaryBankDetailsName
        <*> requiredField' "swiftCode" .= beneficiaryBankDetailsSwiftCode
        <*> requiredField' "routingNumber" .= beneficiaryBankDetailsRoutingNumber
        <*> requiredField' "accountNumber" .= beneficiaryBankDetailsAccountNumber
        <*> requiredField' "currency" .= beneficiaryBankDetailsCurrency
        <*> requiredField' "address" .= beneficiaryBankDetailsAddress
        <*> requiredField' "city" .= beneficiaryBankDetailsCity
        <*> requiredField' "postalCode" .= beneficiaryBankDetailsPostalCode
        <*> requiredField' "country" .= beneficiaryBankDetailsCountry

newtype ProcessorToken = ProcessorToken
  { unProcessorToken :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec ProcessorToken where
  codec = dimapCodec ProcessorToken unProcessorToken codec

newtype AddressLine = AddressLine
  { unAddressLine :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec AddressLine where
  codec = dimapCodec AddressLine unAddressLine codec

newtype URL = URL {unURL :: Text}
  deriving stock (Eq, Show, Lift)
  deriving newtype (ToJSON)

-- From https://daringfireball.net/2010/07/improved_regex_for_matching_urls
urlRegex :: Regex
urlRegex =
  [re|(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))|]

mkURL :: Text -> Maybe URL
mkURL t =
  if t =~ urlRegex
    then Just (URL t)
    else Nothing

urlToText :: URL -> Text
urlToText (URL t) = t

urlToByteString :: URL -> BS8.ByteString
urlToByteString url = TE.encodeUtf8 $ urlToText url

instance FromJSON URL where
  parseJSON = withText "URL" $ \t ->
    case mkURL t of
      Nothing -> fail $ "Invalid URL: " ++ T.unpack t
      Just url -> pure url

instance HasCodec URL where
  codec = dimapCodec URL unURL codec

newtype AccountNumber = AccountNumber {unAccountNumber :: Text}
  deriving stock (Eq, Show, Lift)
  deriving newtype (ToJSON)

-- Account numbers can have capital letters or digits
accountNumberRegex :: Regex
accountNumberRegex = [re|^[A-Z0-9]{4,17}$|]

mkAccountNumber :: Text -> Maybe AccountNumber
mkAccountNumber t =
  if t =~ accountNumberRegex
    then Just (AccountNumber t)
    else Nothing

accountNumberToText :: AccountNumber -> Text
accountNumberToText (AccountNumber t) = t

type AccountNumberMask = Refined (SizeEqualTo 4) Text

accountNumberLastFour :: AccountNumber -> AccountNumberMask
accountNumberLastFour (AccountNumber n) =
  reallyUnsafeRefine
    . T.takeEnd 4
    $ n

accountNumberToByteString :: AccountNumber -> BS8.ByteString
accountNumberToByteString accountNumber = TE.encodeUtf8 $ accountNumberToText accountNumber

instance FromJSON AccountNumber where
  parseJSON = withText "AccountNumber" $ \t ->
    case mkAccountNumber t of
      Nothing -> fail $ "Invalid AccountNumber: " ++ T.unpack t
      Just accountNumber -> pure accountNumber

instance HasCodec AccountNumber where
  codec = dimapCodec AccountNumber unAccountNumber codec

compileAccountNumber :: QuasiQuoter
compileAccountNumber =
  QuasiQuoter
    { quoteExp = compileAccountNumber',
      quotePat = error "AccountNumber is not a pattern; use accountNumberToText instead",
      quoteDec = error "accountNumber is not supported at top-level",
      quoteType = error "accountNumber is not supported as a type"
    }
  where
    compileAccountNumber' :: String -> Q Exp
    compileAccountNumber' s = case mkAccountNumber (T.pack s) of
      Nothing -> fail ("Invalid AccountNumber: " ++ s ++ ". Must be 4-17 digits, with no other characters.")
      Just accountNumber -> [|accountNumber|]

newtype RoutingNumber = RoutingNumber {unRoutingNumber :: Text}
  deriving stock (Show, Lift)
  deriving newtype (Eq, ToJSON)

routingNumberRegex :: Regex
routingNumberRegex = [re|^[0-9]{9}$|]

mkRoutingNumber :: Text -> Maybe RoutingNumber
mkRoutingNumber t =
  if t =~ routingNumberRegex
    then Just (RoutingNumber t)
    else Nothing

routingNumberToText :: RoutingNumber -> Text
routingNumberToText (RoutingNumber t) = t

routingNumberToByteString :: RoutingNumber -> BS8.ByteString
routingNumberToByteString routingNumber = TE.encodeUtf8 $ routingNumberToText routingNumber

instance HasCodec RoutingNumber where
  codec = dimapCodec RoutingNumber unRoutingNumber codec

instance FromJSON RoutingNumber where
  parseJSON = withText "RoutingNumber" $ \t ->
    case mkRoutingNumber t of
      Nothing -> fail $ "Invalid RoutingNumber: " ++ T.unpack t
      Just routingNumber -> pure routingNumber

compileRoutingNumber :: QuasiQuoter
compileRoutingNumber =
  QuasiQuoter
    { quoteExp = compileRoutingNumber',
      quotePat = error "RoutingNumber is not a pattern; use routingNumberToText instead",
      quoteDec = error "routingNumber is not supported at top-level",
      quoteType = error "routingNumber is not supported as a type"
    }
  where
    compileRoutingNumber' :: String -> Q Exp
    compileRoutingNumber' s = case mkRoutingNumber (T.pack s) of
      Nothing -> fail ("Invalid RoutingNumber: " ++ s ++ ". Must be nine digits, with no other characters.")
      Just routingNumber -> [|routingNumber|]

newtype Iban = Iban {unIban :: Text}
  deriving stock (Show, Read, Lift)
  deriving newtype (Eq, ToJSON)

mkIban :: Text -> Maybe Iban
mkIban t = if T.length t <= 34 then Just (Iban t) else Nothing

instance FromJSON Iban where
  parseJSON = withText "Iban" $ \t -> case mkIban t of
    Nothing -> fail $ "Invalid Iban: " ++ T.unpack t
    Just iban -> pure iban

instance HasCodec Iban where
  codec = dimapCodec Iban unIban codec

compileIban :: QuasiQuoter
compileIban =
  QuasiQuoter
    { quoteExp = compileIban',
      quotePat = error "Iban is not a pattern - use `ibanToText` instead",
      quoteDec = error "Iban is not supported at top-level",
      quoteType = error "Iban is not supported as a type"
    }
  where
    compileIban' :: String -> Q Exp
    compileIban' s = case mkIban (T.pack s) of
      Nothing -> fail $ "Invalid Iban: " ++ s
      Just txt -> [|txt|]

ibanToText :: Iban -> Text
ibanToText (Iban t) = t

-- | A newtype around email text.
-- This newtype verifies the email is formatted correctly using HTML5's email regexp https://www.w3.org/TR/html5/forms.html#valid-e-mail-address
newtype Email = Email {getEmailText :: Text}
  deriving stock (Show, Lift)
  deriving newtype (Eq, Ord, ToJSON)

emailRegex :: Regex
emailRegex = [re|^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$|]

-- | Attempt to create an email from text, returning 'Nothing' if it doesn't
-- match our email regular expression.
mkEmail :: Text -> Maybe Email
mkEmail t =
  if t =~ emailRegex
    then Just (Email t)
    else Nothing

-- | Convenience function for APIs that take emails as 'Text'
emailToText :: Email -> Text
emailToText = getEmailText

-- | Convenience function for APIs that take emails as 'ByteString'
emailToByteString :: Email -> BS8.ByteString
emailToByteString email = TE.encodeUtf8 $ emailToText email

instance HasCodec Email where
  codec = dimapCodec Email getEmailText codec

instance FromJSON Email where
  parseJSON (Aeson.String t) = case mkEmail t of
    Nothing -> fail $ "Invalid email address: " ++ show t
    Just email -> pure email
  parseJSON v = fail $ "When trying to parse an Email, expected String, encountered " ++ show v

-- | Create an email at compile time
-- Usage:
-- > [compileEmail|dmarticus@gmail.com|]
compileEmail :: QuasiQuoter
compileEmail =
  QuasiQuoter
    { quoteExp = compileEmail',
      quotePat = error "Email is not a pattern; use `emailToText` instead",
      quoteDec = error "email is not supported at top-level",
      quoteType = error "email is not supported as a type"
    }
  where
    compileEmail' :: String -> Q Exp
    compileEmail' s = case mkEmail (T.pack s) of
      Nothing -> fail ("Invalid Email: " ++ s ++ ". Make sure you aren't wrapping the email in quotes.")
      Just email -> [|email|]

newtype City = City
  { unCity :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec City where
  codec = dimapCodec City unCity codec

-- TODO consider adding validation.  Risk here is that I block valid codes.
newtype PostalCode = PostalCode
  { unPostalCode :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec PostalCode where
  codec = dimapCodec PostalCode unPostalCode codec

newtype SwiftCode = SwiftCode
  { unSwiftCode :: Text
  }
  deriving stock (Show, Lift)
  deriving newtype (Eq, Ord, ToJSON)

swiftCountryCodes :: Set.Set Text
swiftCountryCodes = Set.fromList $ "XK" : (alphaTwoUpper <$> ([minBound .. maxBound] :: [Country]))

swiftCodeRegex :: Regex
swiftCodeRegex = [re|^[A-Z]{6}[A-Z0-9]{2}(?:[A-Z0-9]{3})?$|]

mkSwiftCode :: Text -> Maybe SwiftCode
mkSwiftCode txt = do
  let t = T.toUpper $ T.strip txt
  guard $ t =~ swiftCodeRegex
  guard $ Set.member (getCountryFromSwiftCodeText t) swiftCountryCodes
  pure $ SwiftCode t

instance HasCodec SwiftCode where
  codec = dimapCodec SwiftCode unSwiftCode codec

instance FromJSON SwiftCode where
  parseJSON = withText "SwiftCode" $ \t -> case mkSwiftCode t of
    Nothing -> fail $ "Invalid SwiftCode: " ++ T.unpack t
    Just swift -> pure swift

compileSwiftCode :: QuasiQuoter
compileSwiftCode =
  QuasiQuoter
    { quoteExp = compileSwiftCode',
      quotePat = error "SwiftCode is not a pattern; use swiftCodeToText instead",
      quoteDec = error "SwiftCode is not supported at top-level",
      quoteType = error "SwiftCode is not supported as a type"
    }
  where
    compileSwiftCode' :: String -> Q Exp
    compileSwiftCode' s = case mkSwiftCode (T.pack s) of
      Nothing -> fail $ "Invalid SwiftCode: " ++ s
      Just txt -> [|txt|]

swiftCodeToText :: SwiftCode -> Text
swiftCodeToText (SwiftCode t) = t

-- | Extracts an ISO 3166-1 alpha-2 country code (or XK for Kosovo) from a SWIFT code, e.g. BOFAUS3N -> US
getCountryFromSwiftCodeText :: Text -> Text
getCountryFromSwiftCodeText = T.take 2 . T.drop 4

getCountryFromSwiftCode :: SwiftCode -> Text
getCountryFromSwiftCode = getCountryFromSwiftCodeText . swiftCodeToText

-- | SwiftCodes can be either 11 or 8 digits, this ensures you're always working with the 8 digit variety.
get8DigitSwiftCode :: SwiftCode -> SwiftCode
get8DigitSwiftCode = coerce . T.take 8 . coerce

-- | Gets the branch code from an 11-digit SwiftCode (this is the last 3 digits of an 11-digit SwiftCode), otherwise returns Nothing.
getBranchCodeFromSwiftCode :: SwiftCode -> Maybe Text
getBranchCodeFromSwiftCode swiftCode = do
  let swiftCodeText = swiftCodeToText swiftCode
  guard (T.length swiftCodeText == 11)
  pure (T.drop 8 swiftCodeText)

-- TODO consider adding validation (should district be state?  Can I just use an enum?)
-- District will be a state only if it's US or Canada... will need to be clever here I think.
-- Maybe can have those two fields be modeled with a sum type?
newtype District = District
  { unDistrict :: Text
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec District)

instance HasCodec District where
  codec = dimapCodec District unDistrict codec

-- TODO: Look into making this use Country
newtype ISO3166Alpha2 = ISO3166Alpha2
  { unISO3166Alpha2 :: Text
  }
  deriving newtype (Eq, Show, Ord)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ISO3166Alpha2)

instance HasCodec ISO3166Alpha2 where
  codec = dimapCodec ISO3166Alpha2 unISO3166Alpha2 codec

newtype TrackingReference = TrackingReference
  { unTrackingReference :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec TrackingReference where
  codec = dimapCodec TrackingReference unTrackingReference codec

-- TODO consider adding validation to this type if necessary.
-- The string looks like this 0xcac04f0069e4ac9314ac4e608e99278a3bebabcd
newtype HexString = HexString
  { unHexString :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec HexString where
  codec = dimapCodec HexString unHexString codec

-- TODO consider adding validation if necessary
-- From Circle's docs: "A walletId is a numeric value but should be treated as a string as format may change in the future"
newtype WalletId = WalletId
  { unWalletId :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec WalletId where
  codec = dimapCodec WalletId unWalletId codec

-- TODO what to do about this orphan instance?
instance HasCodec UUID where
  codec = bimapCodec f UUID.toText codec
    where
      f t =
        case UUID.fromText t of
          Nothing -> Left "Invalid Text when parsing UUID"
          Just u -> Right u