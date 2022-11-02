{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Unknot.Types
  ( -- Types for connecting to and wrapping Circle's API
    ApiToken (..),
    Reply,
    Method,
    CircleAPIRequest (..),
    CircleError (..),
    CircleResponseBody (..),
    CircleRequest,
    Host,
    CircleHost (..),
    CircleConfig (..),
    Params (..),
    Body (..),
    Query (..),
    TupleBS8,
    (-&-),
    mkCircleAPIRequest,
    hostUri,
    prodEnvConfig,
    sandboxEnvConfig,
    joinQueryParams,
    -- Query Params
    PaginationQueryParams (..),
    PaginationQueryParam (..),
    -- Wires Endpoint
    WireAccountBodyParams (..),
    WireAccountData (..),
    WireInstructionsData (..),
    WireAccountRequest,
    WireAccountsRequest,
    WireInstructionsRequest,
    -- Signet Bank Endpoints,
    SignetBankAccountRequest,
    SignetBankAccountsRequest,
    SignetBankAccountBodyParams (..),
    SignetBankAccountData (..),
    SignetBankInstructionsData (..),
    -- Balance Endpoint
    BalanceRequest,
    BalanceData (..),
    -- Management Endpoint
    ConfigurationRequest,
    ConfigurationData (..),
    -- Encryption Endpoint
    EncryptionRequest,
    EncryptionData (..),
    -- Channels Endpoint
    ChannelsRequest,
    -- Stablecoins Endpoint
    StablecoinsRequest,
    -- Subscriptions Endpoint
    SubscriptionsRequest,
    SubscriptionRequest,
    SubscriptionBodyParams (..),
    -- Payouts Endpoint
    PayoutsRequest,
    PayoutRequest,
    PayoutData (..),
    PayoutBodyParams (..),
    -- Transfers Endpoint
    TransfersRequest,
    TransferRequest,
    TransferBodyParams (..),
    TransferBodyDestination (..),
    TransferType (..),
    TransferData (..),
    DestinationType (..),
    -- Addresses Endpoint
    DepositAddressesRequest,
    DepositAddressRequest,
    DepositAddressData (..),
    DepositAddressBodyParams (..),
    RecipientAddressesRequest,
    RecipientAddressRequest,
    RecipientAddressData (..),
    RecipientAddressBodyParams (..),
    -- Deposits Endpoints
    DepositsRequest,
    DepositData (..),
    MockSilvergatePaymentRequest,
    MockSilvergatePaymentBodyParams (..),
    MockBeneficiaryBankDetails (..),
    MockSilvergatePaymentData (..),
    -- Silvergate SEN Endpoints
    SENAccountBodyParams (..),
    SENAccountData (..),
    SENInstructionsData (..),
    SENAccountRequest,
    SENAccountsRequest,
    SENInstructionsRequest,
    -- Shared types across different endpoints
    DestinationBankAccount (..),
    HexString (..),
    USDOrEURAmount (..),
    BankAccountType (..),
    AllowedCurrencies (..),
    Chain (..),
    CurrencyAmount (..),
    Amount (..),
    AddressLine (..),
    AccountNumber (..),
    accountNumberLastFour,
    accountNumberRegex,
    accountNumberToByteString,
    accountNumberToText,
    compileAccountNumber,
    mkAccountNumber,
    RoutingNumber (..),
    mkRoutingNumber,
    compileRoutingNumber,
    routingNumberToText,
    routingNumberToByteString,
    City (..),
    PostalCode (..),
    District (..),
    ISO3166Alpha2 (..),
    americanSamoa,
    guam,
    northernMarianaIslands,
    puertoRico,
    unitedStatesMinorOutlyingIslands,
    unitedStatesOfAmerica,
    virginIslandsUs,
    BankAddress (..),
    BillingDetails (..),
    BeneficiaryBankDetails (..),
    BeneficiaryDetails,
    SwiftCode (..),
    mkSwiftCode,
    swiftCodeRegex,
    compileSwiftCode,
    swiftCodeToText,
    getCountryFromSwiftCode,
    get8DigitSwiftCode,
    getBranchCodeFromSwiftCode,
    ResponseStatus (..),
    ResponseMessage (..),
    PayoutErrorCode (..),
    UUID (..),
    TrackingReference (..),
    catThats,
    catThises,
    thisOrThat,
    thisOrThatToEither,
    -- Payments API --
    -- Payments endpoints

  )
where

import Control.Monad (guard)
import Country
  ( Country,
    alphaTwoUpper,
  )
import Country.Identifier (americanSamoa, guam, northernMarianaIslands, puertoRico, unitedStatesMinorOutlyingIslands, unitedStatesOfAmerica, virginIslandsUs)
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Result (Error, Success),
    ToJSON (toEncoding, toJSON),
    Value (Array, Null, String),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (fromJSON)
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Coerce (coerce)
import Data.Fixed (Centi, Fixed (MkFixed))
import Data.Foldable
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.Vector as V
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift)
import Network.HTTP.Client (Response)
import qualified Network.HTTP.Types.Method as NHTM
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

data CircleAPIRequest a b c = CircleAPIRequest
  { -- | Method of CircleAPIRequest
    rMethod :: !Method,
    -- | Endpoint of CircleAPIRequest
    endpoint :: !Text,
    -- | Request params of CircleAPIRequest
    params :: !(Params TupleBS8 BSL.ByteString)
  }
  deriving (Show)

mkCircleAPIRequest ::
  Method ->
  Text ->
  Params TupleBS8 BSL.ByteString ->
  CircleAPIRequest a b c
mkCircleAPIRequest = CircleAPIRequest

type family CircleRequest a :: *

data CircleError = CircleError
  { parseError :: !String, -- TODO should this be Text?
    errorResponseBody :: !Reply
  }
  deriving (Show)

data CircleResponseBody a = CircleResponseBody
  { circleResponseCode :: !(Maybe ResponseStatus),
    circleResponseMessage :: !(Maybe ResponseMessage),
    circleResponseData :: !(Maybe a)
  }
  deriving (Eq, Show)

instance FromJSON a => FromJSON (CircleResponseBody a) where
  parseJSON = withObject "CircleResponseBody" parse
    where
      parse o =
        CircleResponseBody
          <$> o .:? "status"
          <*> o .:? "message"
          <*> o .:? "data"

-- these types have to do with Circle's actual API
newtype ResponseStatus = ResponseStatus
  { unResponseStatus :: Integer
  }
  deriving (Eq, Show, FromJSON)

newtype ResponseMessage = ResponseMessage
  { unResponseMessage :: Text
  }
  deriving (Eq, Show, FromJSON)

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

-- Possibly a bad idea. I don't know
-- why they auth like this.
credentialsEnv :: IO ApiToken
credentialsEnv = do
  token <- getEnv "CIRCLE_API_KEY"
  return (ApiToken $ BS8.pack token)

prodEnvConfig :: IO CircleConfig
prodEnvConfig = do
  CircleConfig CircleProduction <$> credentialsEnv

sandboxEnvConfig :: IO CircleConfig
sandboxEnvConfig = do
  CircleConfig CircleSandbox <$> credentialsEnv

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

-- | Add an optional query parameter
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

data PaginationQueryParam = PageBefore !CircleId | PageAfter !CircleId deriving (Show, Eq) -- TODO these will be IDs, but not UUIDs.  No type until we figure out what they are.

instance ToCircleParam PaginationQueryParams where
  toCircleParam (PaginationQueryParams p) =
    -- Circle has some BS pagination where they let users supply some canonical
    -- collection ID, and then this pagination rule will return `n` entries before OR after that page,
    -- where `n` is controlled by the pageSize param.  This type exists to prevent callers from providing both params, which would error out
    case p of
      PageBefore a ->
        joinQueryParams $ Params Nothing [Query ("pageBefore", TE.encodeUtf8 (unCircleId a))]
      PageAfter a ->
        joinQueryParams $ Params Nothing [Query ("pageAfter", TE.encodeUtf8 (unCircleId a))]

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
    joinQueryParams $ Params Nothing [Query ("pageSize", TE.encodeUtf8 $ (T.pack . show) i)] -- TODO showing seems like a hack.  Better to derive a Display instance.

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
    joinQueryParams $ Params Nothing [Query ("destination", TE.encodeUtf8 (unUUID i))]

newtype TypeQueryParam = TypeQueryParam
  { typeQueryParam :: BankAccountType -- TODO not sure if all of the types should be included.  Should definitely be Wire, but maybe not Sen
  }
  deriving (Eq, Show)

bankAccountTypeToBS8 :: BankAccountType -> BS8.ByteString
bankAccountTypeToBS8 Wire = "wire"
bankAccountTypeToBS8 Sen = "sen"

instance ToCircleParam TypeQueryParam where
  toCircleParam (TypeQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("type", bankAccountTypeToBS8 i)]

newtype CurrencyQueryParam = CurrencyQueryParam
  { currencyQueryParam :: AllowedCurrencies
  }
  deriving (Eq, Show)

currencyToBS8 :: AllowedCurrencies -> BS8.ByteString
currencyToBS8 USD = "USD"
currencyToBS8 EUR = "EUR"
currencyToBS8 BTC' = "BTC"
currencyToBS8 ETH' = "ETH'"

instance ToCircleParam CurrencyQueryParam where
  toCircleParam (CurrencyQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("currency", currencyToBS8 i)]

-- The following types are supported by the Payments API
newtype SourceQueryParam = SourceQueryParam
  { sourceQueryParam :: UUID
  }
  deriving (Eq, Show)

instance ToCircleParam SourceQueryParam where
  toCircleParam (SourceQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("source", TE.encodeUtf8 (unUUID i))]

newtype SettlementIdQueryParam = SettlementIdQueryParam
  { settlementIdQueryParam :: UUID
  }
  deriving (Eq, Show)

instance ToCircleParam SettlementIdQueryParam where
  toCircleParam (SettlementIdQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("settlementId", TE.encodeUtf8 (unUUID i))]

newtype PaymentIntentIdQueryParam = PaymentIntentIdQueryParam
  { paymentIntentIdQueryParam :: UUID
  }
  deriving (Eq, Show)

instance ToCircleParam PaymentIntentIdQueryParam where
  toCircleParam (PaymentIntentIdQueryParam i) =
    joinQueryParams $ Params Nothing [Query ("paymentIntentId", TE.encodeUtf8 (unUUID i))]

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
---------------------------------------------------------------
-- Balance endpoints
---------------------------------------------------------------

data BalanceRequest

type instance CircleRequest BalanceRequest = CircleResponseBody BalanceData

data BalanceData = BalanceData
  { balanceDataAvailable :: ![CurrencyAmount],
    balanceDataUnsettled :: ![CurrencyAmount]
  }
  deriving (Show)

instance FromJSON BalanceData where
  parseJSON = withObject "BalanceData" parse
    where
      parse o =
        BalanceData
          <$> o .: "available"
          <*> o .: "unsettled"

data CurrencyAmount = CurrencyAmount
  { currencyAmountAmount :: !Amount,
    currencyAmountCurrency :: !AllowedCurrencies
  }
  deriving (Eq, Show)

instance FromJSON CurrencyAmount where
  parseJSON = withObject "CurrencyAmount" parse
    where
      parse o =
        CurrencyAmount
          <$> o .: "amount"
          <*> o .: "currency"

instance ToJSON CurrencyAmount where
  toJSON CurrencyAmount {..} =
    object
      [ "amount" .= currencyAmountAmount,
        "currency" .= currencyAmountCurrency
      ]

---------------------------------------------------------------
-- Payout endpoints
---------------------------------------------------------------
data PayoutRequest

type instance CircleRequest PayoutRequest = CircleResponseBody PayoutData

data PayoutsRequest

type instance CircleRequest PayoutsRequest = CircleResponseBody [PayoutData]

instance CircleHasParam PayoutsRequest PaginationQueryParams

instance CircleHasParam PayoutsRequest FromQueryParam

instance CircleHasParam PayoutsRequest ToQueryParam

instance CircleHasParam PayoutsRequest PageSizeQueryParam

instance CircleHasParam PayoutsRequest StatusQueryParams

instance CircleHasParam PayoutsRequest TypeQueryParam

instance CircleHasParam PayoutsRequest DestinationQueryParam

data PayoutReturn = PayoutReturn
  { payoutReturnId :: !UUID,
    payoutReturnOriginalPayoutId :: !UUID,
    payoutReturnAmount :: !USDAmount,
    payoutReturnFees :: !USDAmount,
    payoutReturnReason :: !Text,
    payoutReturnStatus :: !Status,
    payoutReturnCreateDate :: !UTCTime,
    payoutReturnUpdateDate :: !UTCTime
  }
  deriving (Show)

instance FromJSON PayoutReturn where
  parseJSON = withObject "PayoutReturn" parse
    where
      parse o =
        PayoutReturn
          <$> o .: "id"
          <*> o .: "payoutId"
          <*> o .: "amount"
          <*> o .: "fees"
          <*> o .: "reason"
          <*> o .: "status"
          <*> o .: "createDate"
          <*> o .: "updateDate"

data PayoutData = PayoutData
  { payoutDataId :: !CircleId,
    payoutDataSourceWalletId :: !WalletId,
    payoutDataDestinationBankAccount :: !DestinationBankAccount,
    payoutDataAmount :: !USDOrEURAmount,
    payoutDataFees :: !USDAmount,
    payoutDataStatus :: !Status,
    payoutDataTrackingRef :: !TrackingReference, -- TODO maybe this needs a custom type, although text is probably fine
    payoutDataPayoutErrorCode :: !PayoutErrorCode,
    payoutDataRiskEvaluation :: !RiskEvaluation,
    payoutDataAdjustments :: !Adjustments,
    payoutDataPayoutReturn :: !PayoutReturn,
    payoutDataCreateDate :: !UTCTime,
    payoutDataUpdateDate :: !UTCTime
  }
  deriving (Show)

instance FromJSON PayoutData where
  parseJSON = withObject "PayoutData" parse
    where
      parse o =
        PayoutData
          <$> o .: "id"
          <*> o .: "sourceWalletId"
          <*> o .: "destination"
          <*> o .: "amount"
          <*> o .: "fees"
          <*> o .: "status"
          <*> o .: "trackingRef"
          <*> o .: "errorCode"
          <*> o .: "riskEvaluation"
          <*> o .: "adjustments"
          <*> o .: "payoutReturn"
          <*> o .: "createDate"
          <*> o .: "updateDate"

data PayoutBodyParams = PayoutBodyParams
  { payoutBodyParamsIdempotencyKey :: !UUID,
    payoutBodyParamsDestination :: !DestinationBankAccount,
    payoutBodyParamsAmount :: !USDOrEURAmount
  }
  deriving (Eq, Show)

instance ToJSON PayoutBodyParams where
  toJSON PayoutBodyParams {..} =
    object
      [ "idempotencyKey" .= payoutBodyParamsIdempotencyKey,
        "destination" .= payoutBodyParamsDestination,
        "amount" .= payoutBodyParamsAmount
      ]

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

instance ToJSON PayoutErrorCode where
  toJSON InsufficientFunds = String "insufficient_funds"
  toJSON TransactionDenied = String "transaction_denied"
  toJSON TransactionFailed = String "transaction_failed"
  toJSON TransactionReturned = String "transaction_returned"
  toJSON BankTransactionError = String "bank_transaction_error"
  toJSON FiatAccountLimitExceeded = String "fiat_account_limit_exceeded"
  toJSON InvalidBankAccountNumber = String "invalid_bank_account_number"
  toJSON InvalidACHRoutingTransitNumber = String "invalid_ach_rtn"
  toJSON InvalidWireRoutingTransitNumber = String "invalid_wire_rtn"
  toJSON VendorInactive = String "vendor_inactive"

instance FromJSON PayoutErrorCode where
  parseJSON (String s) = case T.unpack s of
    "insufficient_funds" -> return InsufficientFunds
    "transaction_denied" -> return TransactionDenied
    "transaction_failed" -> return TransactionFailed
    "transaction_returned" -> return TransactionReturned
    "bank_transaction_error" -> return BankTransactionError
    "fiat_account_limit_exceeded" -> return FiatAccountLimitExceeded
    "invalid_bank_account_number" -> return InvalidBankAccountNumber
    "invalid_ach_rtn" -> return InvalidACHRoutingTransitNumber
    "invalid_wire_rtn" -> return InvalidWireRoutingTransitNumber
    "vendor_inactive" -> return VendorInactive
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

---------------------------------------------------------------
-- Management endpoint
---------------------------------------------------------------

data ConfigurationRequest

type instance CircleRequest ConfigurationRequest = CircleResponseBody ConfigurationData

newtype ConfigurationData = ConfigurationData {configurationDataPayments :: WalletConfig}
  deriving (Eq, Show)

instance FromJSON ConfigurationData where
  parseJSON = withObject "ConfigurationData" parse
    where
      parse o =
        ConfigurationData
          <$> o .: "payments"

newtype WalletConfig = WalletConfig {masterWalletId :: WalletId}
  deriving (Eq, Show)

instance FromJSON WalletConfig where
  parseJSON = withObject "WalletConfig" parse
    where
      parse o =
        WalletConfig
          <$> o .: "masterWalletId"

---------------------------------------------------------------
-- Encryption endpoint
---------------------------------------------------------------

data EncryptionRequest

type instance CircleRequest EncryptionRequest = CircleResponseBody EncryptionData

data EncryptionData = EncryptionData
  { encryptionDataKeyId :: !Text, -- TODO this should actually be a UUID, but for the tests to work it needs to be relaxed a bit
    encryptionDataPublicKey :: !Text -- TODO is there a PGP key type in Haskell?
  }
  deriving (Eq, Show)

instance FromJSON EncryptionData where
  parseJSON = withObject "EncryptionData" parse
    where
      parse o =
        EncryptionData
          <$> o .: "keyId"
          <*> o .: "publicKey"

---------------------------------------------------------------
-- Channels endpoint
---------------------------------------------------------------

data ChannelsRequest

type instance CircleRequest ChannelsRequest = CircleResponseBody ChannelData

newtype ChannelData = ChannelData {channels :: [Channel]}
  deriving (Eq, Show)

instance FromJSON ChannelData where
  parseJSON = withObject "ChannelData" parse
    where
      parse o =
        ChannelData
          <$> o .: "channels"

data Channel = Channel
  { channelId :: !CircleId,
    channelDefault :: !Bool,
    channelCardDescriptor :: !Text,
    channelAchDescriptor :: !Text
  }
  deriving (Eq, Show)

instance FromJSON Channel where
  parseJSON = withObject "Channel" parse
    where
      parse o =
        Channel
          <$> o .: "id"
          <*> o .: "default"
          <*> o .: "cardDescriptor"
          <*> o .: "achDescriptor"

---------------------------------------------------------------
-- Stablecoins endpoint
---------------------------------------------------------------

data StablecoinsRequest

type instance CircleRequest StablecoinsRequest = CircleResponseBody [StablecoinData]

data StablecoinData = StablecoinData
  { stablecoinDataName :: !Text,
    stablecoinDataSymbol :: !Stablecoin,
    stablecoinDataTotalAmount :: !Text, --TODO need an amount field that supports crypto depth.  Many of these coins go past a fixed integer depth
    stablecoinDataChains :: ![ChainAmount]
  }
  deriving (Eq, Show)

instance FromJSON StablecoinData where
  parseJSON = withObject "StablecoinData" parse
    where
      parse o =
        StablecoinData
          <$> o .: "name"
          <*> o .: "symbol"
          <*> o .: "totalAmount"
          <*> o .: "chains"

data ChainAmount = ChainAmount
  { chainAmountAmount :: !Text,
    chainAmountChain :: !Chain,
    chainAmountUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)

instance FromJSON ChainAmount where
  parseJSON = withObject "ChainAmount" parse
    where
      parse o =
        ChainAmount
          <$> o .: "amount"
          <*> o .: "chain"
          <*> o .: "updateDate"

data Chain = ALGO | AVAX | BTC | ETH | FLOW | HBAR | MATIC | SOL | TRX | XLM deriving (Eq, Show)

instance ToJSON Chain where
  toJSON ALGO = String "ALGO"
  toJSON AVAX = String "AVAX"
  toJSON BTC = String "BTC"
  toJSON ETH = String "ETH"
  toJSON FLOW = String "FLOW"
  toJSON HBAR = String "HBAR"
  toJSON MATIC = String "MATIC"
  toJSON SOL = String "SOL"
  toJSON TRX = String "TRX"
  toJSON XLM = String "XLM"

instance FromJSON Chain where
  parseJSON (String s) = case T.unpack s of
    "ALGO" -> return ALGO
    "AVAX" -> return AVAX
    "BTC" -> return BTC
    "ETH" -> return ETH
    "FLOW" -> return FLOW
    "HBAR" -> return HBAR
    "MATIC" -> return MATIC
    "SOL" -> return SOL
    "TRX" -> return TRX
    "XLM" -> return XLM
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

data Stablecoin = USDC | EUROC | USDT deriving (Eq, Show)

instance ToJSON Stablecoin where
  toJSON USDC = String "USDC"
  toJSON EUROC = String "EUROC"
  toJSON USDT = String "USDT"

instance FromJSON Stablecoin where
  parseJSON (String s) = case T.unpack s of
    "USDC" -> return USDC
    "EUROC" -> return EUROC
    "USDT" -> return USDT
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

---------------------------------------------------------------
-- Subscription endpoints
---------------------------------------------------------------

data SubscriptionsRequest

type instance CircleRequest SubscriptionsRequest = CircleResponseBody [SubscriptionData]

data SubscriptionRequest

type instance CircleRequest SubscriptionRequest = CircleResponseBody SubscriptionData

data SubscriptionData = SubscriptionData
  { subscriptionDataId :: !Text,
    subscriptionDataEndpoint :: !Text,
    subscriptionDataSubscriptionDetails :: ![SubscriptionDetails]
  }
  deriving (Eq, Show)

instance FromJSON SubscriptionData where
  parseJSON = withObject "SubscriptionData" parse
    where
      parse o =
        SubscriptionData
          <$> o .: "id"
          <*> o .: "endpoint"
          <*> o .: "subscriptionDetails"

data SubscriptionDetails = SubscriptionDetails
  { subscriptionDetailsUrl :: !Text,
    subscriptionDetailsStatus :: !Status
  }
  deriving (Eq, Show)

instance FromJSON SubscriptionDetails where
  parseJSON = withObject "SubscriptionDetails" parse
    where
      parse o =
        SubscriptionDetails
          <$> o .: "url"
          <*> o .: "status"

newtype SubscriptionBodyParams = SubscriptionBodyParams
  { subscriptionBodyParamsEndpoint :: Text
  }
  deriving (Eq, Show)

instance ToJSON SubscriptionBodyParams where
  toJSON SubscriptionBodyParams {..} =
    object
      [ "endpoint" .= subscriptionBodyParamsEndpoint
      ]

---------------------------------------------------------------
-- Transfer endpoints
---------------------------------------------------------------

data TransfersRequest

type instance CircleRequest TransfersRequest = CircleResponseBody [TransferData]

instance CircleHasParam TransfersRequest PaginationQueryParams

instance CircleHasParam TransfersRequest FromQueryParam

instance CircleHasParam TransfersRequest ToQueryParam

instance CircleHasParam TransfersRequest PageSizeQueryParam

data TransferRequest

type instance CircleRequest TransferRequest = CircleResponseBody TransferData

data TransferBodyParams = TransferBodyParams
  { transferBodyParamsId :: !UUID,
    transferBodyParamsDestination :: !TransferBodyDestination,
    transferBodyParamsAmount :: !CurrencyAmount
  }

instance ToJSON TransferBodyParams where
  toJSON TransferBodyParams {..} =
    object
      [ "idempotencyKey" .= transferBodyParamsId,
        "destination" .= transferBodyParamsDestination,
        "amount" .= transferBodyParamsAmount
      ]

data TransferBodyDestination = TransferBodyDestination
  { transferBodyDestinationType :: !DestinationType,
    transferBodyDestinationAddressId :: !UUID
  }
  deriving (Eq, Show)

instance ToJSON TransferBodyDestination where
  toJSON TransferBodyDestination {..} =
    object
      [ "type" .= transferBodyDestinationType,
        "addressId" .= transferBodyDestinationAddressId
      ]

data DestinationType = VerifiedBlockchain deriving (Eq, Show)

instance ToJSON DestinationType where
  toJSON VerifiedBlockchain = String "verified_blockchain"

data TransferData = TransferData
  { transferDataId :: !CircleId,
    transferDataSource :: !(ThisOrThat SourceWallet SourceBlockchain),
    transferDataDestination :: !(ThisOrThat DestinationWallet DestinationBlockchain),
    transferDataAmount :: !CurrencyAmount,
    transferDataFees :: !(Maybe USDAmount),
    transferDataTransactionHash :: !(Maybe HexString),
    transferDataStatus :: !Status,
    transferDataTransferErrorCode :: !(Maybe TransferErrorCode),
    transferDataCreateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

instance FromJSON TransferData where
  parseJSON = withObject "TransferData" parse
    where
      parse o =
        TransferData
          <$> o .: "id"
          <*> o .: "source"
          <*> o .: "destination"
          <*> o .: "amount"
          <*> o .: "fees"
          <*> o .: "transactionHash"
          <*> o .: "status"
          <*> o .: "errorCode"
          <*> o .: "createDate"

data SourceWallet = SourceWallet
  { sourceWalletType :: !TransferType,
    sourceWalletId :: !WalletId, -- From Circle's docs: "Numeric value but should be treated as a string as format may change in the future"
    sourceWalletIdentities :: ![Identity]
  }
  deriving (Eq, Show)

instance FromJSON SourceWallet where
  parseJSON = withObject "SourceWallet" parse
    where
      parse o =
        SourceWallet
          <$> o .: "type"
          <*> o .: "id"
          <*> o .: "identities"

data SourceBlockchain = SourceBlockchain
  { sourceBlockchainType :: !TransferType,
    sourceBlockchainChain :: !Chain,
    sourceBlockChainIdentities :: ![Identity]
  }
  deriving (Eq, Show)

instance FromJSON SourceBlockchain where
  parseJSON = withObject "SourceBlockchain" parse
    where
      parse o =
        SourceBlockchain
          <$> o .: "type"
          <*> o .: "chain"
          <*> o .: "identities"

data DestinationWallet = DestinationWallet
  { destinationWalletType :: !TransferType,
    destinationWalletId :: !WalletId,
    destinationWalletAddress :: !(Maybe HexString),
    destinationWalletAddressTag :: !(Maybe Text)
  }
  deriving (Eq, Show)

instance FromJSON DestinationWallet where
  parseJSON = withObject "DestinationWallet" parse
    where
      parse o =
        DestinationWallet
          <$> o .: "type"
          <*> o .: "id"
          <*> o .: "address"
          <*> o .: "addressTag"

data DestinationBlockchain = DestinationBlockchain
  { destinationBlockchainType :: !TransferType,
    destinationBlockchainAddress :: !Text,
    destinationBlockchainAddressTag :: !(Maybe CircleId),
    destinationBlockchainAddressChain :: !Chain
  }
  deriving (Eq, Show)

instance FromJSON DestinationBlockchain where
  parseJSON = withObject "DestinationBlockchain" parse
    where
      parse o =
        DestinationBlockchain
          <$> o .: "type"
          <*> o .: "address"
          <*> o .: "addressTag"
          <*> o .: "chain"

data Identity = Identity
  { identityType :: !IdentityType,
    identityName :: !Text,
    identityAddresses :: ![Address]
  }
  deriving (Eq, Show)

instance FromJSON Identity where
  parseJSON = withObject "Identity" parse
    where
      parse o =
        Identity
          <$> o .: "type"
          <*> o .: "name"
          <*> o .: "addresses"

data IdentityType = Individual | Business deriving (Eq, Show)

instance FromJSON IdentityType where
  parseJSON (String s) = case T.unpack s of
    "individual" -> return Individual
    "business" -> return Business
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

data TransferType = Wallet | Blockchain deriving (Eq, Show)

instance FromJSON TransferType where
  parseJSON (String s) = case T.unpack s of
    "wallet" -> return Wallet
    "blockchain" -> return Blockchain
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

data TransferErrorCode
  = InsufficientFunds'
  | BlockchainError
  | TransferDenied
  | TransferFailed
  deriving (Eq, Show)

instance ToJSON TransferErrorCode where
  toJSON InsufficientFunds' = String "insufficient_funds"
  toJSON BlockchainError = String "blockchain_error"
  toJSON TransferDenied = String "transfer_denied"
  toJSON TransferFailed = String "transfer_failed"

instance FromJSON TransferErrorCode where
  parseJSON (String s) = case T.unpack s of
    "insufficient_funds" -> return InsufficientFunds'
    "blockchain_error" -> return BlockchainError
    "transfer_denied" -> return TransferDenied
    "transfer_failed" -> return TransferFailed
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

---------------------------------------------------------------
-- Address endpoints
---------------------------------------------------------------

data DepositAddressesRequest

type instance CircleRequest DepositAddressesRequest = CircleResponseBody [DepositAddressData]

data DepositAddressRequest

type instance CircleRequest DepositAddressRequest = CircleResponseBody DepositAddressData

data DepositAddressData = DepositAddressData
  { depositAddressAddress :: !HexString,
    depositAddressAddressTag :: !(Maybe CircleId), -- The docs say it's on the response, but the sandbox API doesn't return in. Make it a `Maybe` for now.
    depositAddressCurrency :: !AllowedCurrencies,
    depositAddressChain :: !Chain
  }
  deriving (Eq, Show)

instance FromJSON DepositAddressData where
  parseJSON = withObject "DepositAddressData" parse
    where
      parse o =
        DepositAddressData
          <$> o .: "address"
          <*> o .:? "addressTag"
          <*> o .: "currency"
          <*> o .: "chain"

data DepositAddressBodyParams = DepositAddressBodyParams
  { depositAddressBodyIdempotencyKey :: !UUID,
    depositAddressBodyCurrency :: !AllowedCurrencies,
    depositAddressBodyChain :: !Chain
  }
  deriving (Eq, Show)

instance ToJSON DepositAddressBodyParams where
  toJSON DepositAddressBodyParams {..} =
    object
      [ "idempotencyKey" .= depositAddressBodyIdempotencyKey,
        "currency" .= depositAddressBodyCurrency,
        "chain" .= depositAddressBodyChain
      ]

data RecipientAddressesRequest

type instance CircleRequest RecipientAddressesRequest = CircleResponseBody [RecipientAddressData]

instance CircleHasParam RecipientAddressesRequest PaginationQueryParams

instance CircleHasParam RecipientAddressesRequest FromQueryParam

instance CircleHasParam RecipientAddressesRequest ToQueryParam

instance CircleHasParam RecipientAddressesRequest PageSizeQueryParam

data RecipientAddressRequest

type instance CircleRequest RecipientAddressRequest = CircleResponseBody RecipientAddressData

data RecipientAddressData = RecipientAddressData
  { recipientAddressId :: !CircleId,
    recipientAddressAddress :: !HexString,
    recipientAddressAddressTag :: !(Maybe CircleId),
    recipientAddressChain :: !Chain,
    recipientAddressCurrency :: !AllowedCurrencies,
    recipientAddressDescription :: !Text
  }
  deriving (Eq, Show)

instance FromJSON RecipientAddressData where
  parseJSON = withObject "RecipientAddressData" parse
    where
      parse o =
        RecipientAddressData
          <$> o .: "id"
          <*> o .: "address"
          <*> o .:? "addressTag"
          <*> o .: "chain"
          <*> o .: "currency"
          <*> o .: "description"

data RecipientAddressBodyParams = RecipientAddressBodyParams
  { recipientAddressBodyIdempotencyKey :: !UUID,
    recipientAddressBodyAddress :: !HexString,
    recipientAddressBodyAddressTag :: !(Maybe CircleId),
    recipientAddressBodyChain :: !Chain,
    recipientAddressBodyCurrency :: !AllowedCurrencies,
    recipientAddressBodyDescription :: !Text
  }
  deriving (Eq, Show)

instance ToJSON RecipientAddressBodyParams where
  toJSON RecipientAddressBodyParams {..} =
    omitNulls
      [ "idempotencyKey" .= recipientAddressBodyIdempotencyKey,
        "address" .= recipientAddressBodyAddress,
        "addressTag" .= recipientAddressBodyAddressTag,
        "chain" .= recipientAddressBodyChain,
        "currency" .= recipientAddressBodyCurrency,
        "description" .= recipientAddressBodyDescription
      ]

---------------------------------------------------------------
-- Deposits endpoints
---------------------------------------------------------------

data DepositsRequest

type instance CircleRequest DepositsRequest = CircleResponseBody [DepositData]

instance CircleHasParam DepositsRequest TypeQueryParam

instance CircleHasParam DepositsRequest PaginationQueryParams

instance CircleHasParam DepositsRequest FromQueryParam

instance CircleHasParam DepositsRequest ToQueryParam

instance CircleHasParam DepositsRequest PageSizeQueryParam

data DepositData = DepositData
  { depositId :: !CircleId,
    depositSourceWalletId :: !(Maybe WalletId),
    depositDestination :: !DestinationWallet,
    depositAmount :: !CurrencyAmount,
    depositFee :: !(Maybe USDAmount),
    depositStatus :: !Status,
    depositRiskEvaluation :: !(Maybe RiskEvaluation),
    depositCreateDate :: !UTCTime,
    depositUpdateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

instance FromJSON DepositData where
  parseJSON = withObject "DepositData" parse
    where
      parse o =
        DepositData
          <$> o .: "id"
            <*> o .:? "sourceWalletId"
            <*> o .: "destination"
            <*> o .: "amount"
            <*> o .:? "fee"
            <*> o .: "status"
            <*> o .:? "riskEvaluation"
            <*> o .: "createDate"
            <*> o .:? "updateDate"

-- TODO is this real?  Like should I make some real code that does what this mock does and expose that in the module?
data MockSilvergatePaymentRequest

type instance CircleRequest MockSilvergatePaymentRequest = CircleResponseBody MockSilvergatePaymentData

data MockSilvergatePaymentData = MockSilvergatePaymentData
  { mockSilvergatePaymentDataTrackingRef :: !TrackingReference,
    mockSilvergatePaymentDataAmount :: !CurrencyAmount,
    mockSilvergatePaymentDataBeneficiaryBank :: !BeneficiaryBankDetails,
    mockSilvergatePaymentDataStatus :: !Status
  }
  deriving (Eq, Show)

instance FromJSON MockSilvergatePaymentData where
  parseJSON = withObject "MockSilvergatePaymentData" parse
    where
      parse o =
        MockSilvergatePaymentData
          <$> o .: "trackingRef"
          <*> o .: "amount"
          <*> o .: "beneficiaryBank"
          <*> o .: "status"

newtype MockBeneficiaryBankDetails = MockBeneficiaryBankDetails {mockBeneficiaryBankDetailsAccountNumber :: AccountNumber}
  deriving (Eq, Show, ToJSON, FromJSON)

-- instance FromJSON MockBeneficiaryBankDetails where
--   parseJSON = withObject "MockBeneficiaryBankDetails" parse
--     where
--       parse o =
--         mockBeneficiaryBankDetails
--           <$> o .: "accountNumber"

-- instance ToJSON MockBeneficiaryBankDetails where
--   toJSON MockBeneficiaryBankDetails {..} =
--     object
--       [ "accountNumber" .= mockBeneficiaryBankDetailsAccountNumber
--       ]

-- TODO this is really close to the return type.  Try to combine at some point?
data MockSilvergatePaymentBodyParams = MockSilvergatePaymentBodyParams
  { mockSilvergatePaymentBodyParamsTrackingRef :: !TrackingReference,
    mockSilvergatePaymentBodyParamsAmount :: !CurrencyAmount,
    -- TODO should this be the real beneficiary bank details object?  The API docs don't say so but the API docs also fail
    mockSilvergatePaymentBodyParamsBeneficiaryBank :: !MockBeneficiaryBankDetails
  }
  deriving (Eq, Show)

instance ToJSON MockSilvergatePaymentBodyParams where
  toJSON MockSilvergatePaymentBodyParams {..} =
    object
      [ "trackingRef" .= mockSilvergatePaymentBodyParamsTrackingRef,
        "amount" .= mockSilvergatePaymentBodyParamsAmount,
        "beneficiaryBank" .= mockSilvergatePaymentBodyParamsBeneficiaryBank
      ]

---------------------------------------------------------------
-- Silvergate SEN endpoints
---------------------------------------------------------------

data SENAccountRequest

type instance CircleRequest SENAccountRequest = CircleResponseBody SENAccountData

data SENAccountsRequest

type instance CircleRequest SENAccountsRequest = CircleResponseBody [SENAccountData]

data SENInstructionsRequest

type instance CircleRequest SENInstructionsRequest = CircleResponseBody SENInstructionsData

data SENAccountBodyParams = SENAccountBodyParams
  { senAccountBodyParamsIdempotencyKey :: !UUID,
    senAccountBodyParamsAccountNumber :: !AccountNumber,
    senAccountBodyParamsCurrency :: !(Maybe AllowedCurrencies)
  }
  deriving (Eq, Show)

instance ToJSON SENAccountBodyParams where
  toJSON SENAccountBodyParams {..} =
    omitNulls
      [ "idempotencyKey" .= senAccountBodyParamsIdempotencyKey,
        "accountNumber" .= senAccountBodyParamsAccountNumber,
        "currency" .= senAccountBodyParamsCurrency
      ]

data SENAccountData = SENAccountData
  { senAccountDataId :: !UUID,
    senAccountDataStatus :: !Status,
    senAccountDataDescription :: !Text, -- TODO better type: Bank name plus last four digits of the bank account number or IBAN.  Make a custom type for this
    senAccountDataTrackingRef :: !TrackingReference,
    senAccountDataCreateDate :: !UTCTime,
    senAccountDataUpdateDate :: !UTCTime,
    senAccountDataCurrency :: !(Maybe AllowedCurrencies)
  }
  deriving (Eq, Show)

instance FromJSON SENAccountData where
  parseJSON = withObject "SENAccountData" parse
    where
      parse o =
        SENAccountData
          <$> o .: "id"
          <*> o .: "status"
          <*> o .: "description"
          <*> o .: "trackingRef"
          <*> o .: "createDate"
          <*> o .: "updateDate"
          <*> o .:? "currency"

data SENInstructionsData = SENInstructionsData
  { senInstructionsDataTrackingRef :: !TrackingReference,
    senInstructionsDataAccountNumber :: !AccountNumber,
    senInstructionsDataCurrency :: !AllowedCurrencies
  }
  deriving (Eq, Show)

instance FromJSON SENInstructionsData where
  parseJSON = withObject "SENInstructionsData" parse
    where
      parse o =
        SENInstructionsData
          <$> o .: "trackingRef"
          <*> o .: "accountNumber"
          <*> o .: "currency"

---------------------------------------------------------------
-- Signet endpoints
---------------------------------------------------------------

data SignetBankAccountRequest

type instance CircleRequest SignetBankAccountRequest = CircleResponseBody SignetBankAccountData

data SignetBankAccountsRequest

type instance CircleRequest SignetBankAccountsRequest = CircleResponseBody [SignetBankAccountData]

data SignetBankInstructionsRequest

type instance CircleRequest SignetBankInstructionsRequest = CircleResponseBody SignetBankInstructionsData

data SignetBankAccountBodyParams = SignetBankAccountBodyParams
  { signetBankAccountBodyParamsIdempotencyKey :: !UUID,
    signetBankAccountBodyParamsWalletAddress :: !HexString
  }
  deriving (Eq, Show)

instance ToJSON SignetBankAccountBodyParams where
  toJSON SignetBankAccountBodyParams {..} =
    object
      [ "idempotencyKey" .= signetBankAccountBodyParamsIdempotencyKey,
        "walletAddress" .= signetBankAccountBodyParamsWalletAddress
      ]

data SignetBankAccountData = SignetBankAccountData
  { signetBankAccountId :: !CircleId,
    signetBankAccountStatus :: !Status,
    signetBankAccountTrackingRef :: !TrackingReference,
    signetBankAccountWalletAddress :: !HexString,
    signetBankAccountCreateDate :: !UTCTime,
    signetBankAccountUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)

instance FromJSON SignetBankAccountData where
  parseJSON = withObject "SignetBankAccountData" parse
    where
      parse o =
        SignetBankAccountData
          <$> o .: "id"
          <*> o .: "status"
          <*> o .: "trackingRef"
          <*> o .: "walletAddress"
          <*> o .: "createDate"
          <*> o .: "updateDate"

data SignetBankInstructionsData = SignetBankInstructionsData
  { signetBankInstructionsTrackingRef :: !(Maybe TrackingReference),
    signetBankInstructionsWalletAddress :: !(Maybe HexString) -- TODO this should have a type, looks like this 0xcac04f0069e4ac9314ac4e608e99278a3bebabcd
  }
  deriving (Eq, Show)

instance FromJSON SignetBankInstructionsData where
  parseJSON = withObject "SignetBankInstructionsData" parse
    where
      parse o =
        SignetBankInstructionsData
          <$> o .:? "trackingRef"
          <*> o .:? "walletAddress"

---------------------------------------------------------------
-- Wire endpoints
---------------------------------------------------------------

data WireAccountRequest

type instance CircleRequest WireAccountRequest = CircleResponseBody WireAccountData

data WireAccountsRequest

type instance CircleRequest WireAccountsRequest = CircleResponseBody [WireAccountData]

data WireInstructionsRequest

type instance CircleRequest WireInstructionsRequest = CircleResponseBody WireInstructionsData

instance CircleHasParam WireInstructionsRequest PaginationQueryParams

data WireAccountBodyParams = WireAccountBodyParams
  { wireAccountBodyParamsIdempotencyKey :: !UUID,
    wireAccountBodyParamsAccountNumber :: !AccountNumber,
    wireAccountBodyParamsRoutingNumber :: !RoutingNumber,
    wireAccountBodyParamsBillingDetails :: !BillingDetails,
    wireAccountBodyParamsBankAddress :: !BankAddress
  }
  deriving (Eq, Show)

instance ToJSON WireAccountBodyParams where
  toJSON WireAccountBodyParams {..} =
    object
      [ "idempotencyKey" .= wireAccountBodyParamsIdempotencyKey,
        "accountNumber" .= wireAccountBodyParamsAccountNumber,
        "routingNumber" .= wireAccountBodyParamsRoutingNumber,
        "billingDetails" .= wireAccountBodyParamsBillingDetails,
        "bankAddress" .= wireAccountBodyParamsBankAddress
      ]

data WireInstructionsData = WireInstructionsData
  { wireInstructionsDataTrackingRef :: !TrackingReference,
    wireInstructionsDataBeneficiaryDetails :: !BeneficiaryDetails,
    wireInstructionsDataBeneficiaryBankDetails :: !BeneficiaryBankDetails
  }
  deriving (Eq, Show)

instance FromJSON WireInstructionsData where
  parseJSON = withObject "WireInstructionsData" parse
    where
      parse o =
        WireInstructionsData
          <$> o .: "trackingRef"
          <*> o .: "beneficiary"
          <*> o .: "beneficiaryBank"

data WireAccountData = WireAccountData
  { wireAccountDataId :: !UUID,
    wireAccountDataStatus :: !Status,
    wireAccountDataDescription :: !Text, -- TODO better type: Bank name plus last four digits of the bank account number or IBAN.  Make a custom type for this
    wireAccountDataTrackingRef :: !TrackingReference,
    wireAccountDataFingerprint :: !UUID,
    wireAccountDataBillingDetails :: !BillingDetails,
    wireAccountDataBankAddress :: !BankAddress,
    wireAccountDataCreateDate :: !UTCTime,
    wireAccountDataUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)

instance FromJSON WireAccountData where
  parseJSON = withObject "WireAccountData" parse
    where
      parse o =
        WireAccountData
          <$> o .: "id"
          <*> o .: "status"
          <*> o .: "description"
          <*> o .: "trackingRef"
          <*> o .: "fingerprint"
          <*> o .: "billingDetails"
          <*> o .: "bankAddress"
          <*> o .: "createDate"
          <*> o .: "updateDate"

---------------------------------------------------------------
-- Shared types
---------------------------------------------------------------
data Status = Pending | Complete | Failed deriving (Show, Eq)

instance ToJSON Status where
  toJSON Pending = String "pending"
  toJSON Complete = String "complete"
  toJSON Failed = String "failed"

instance FromJSON Status where
  parseJSON (String s) = case T.unpack s of
    "pending" -> return Pending
    "complete" -> return Complete
    "failed" -> return Failed
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

data PaymentStatus = PaymentPending | Confirmed | Paid | PaymentFailed | ActionRequired deriving (Show, Eq)

instance ToJSON PaymentStatus where
  toJSON PaymentPending = String "pending"
  toJSON Confirmed = String "confirmed"
  toJSON Paid = String "paid"
  toJSON PaymentFailed = String "failed"
  toJSON ActionRequired = String "action_required"

instance FromJSON PaymentStatus where
  parseJSON (String s) = case T.unpack s of
    "pending" -> return PaymentPending
    "confirmed" -> return Confirmed
    "paid" -> return Paid
    "failed" -> return PaymentFailed
    "action_required" -> return ActionRequired
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

data Address = Address
  { addressCity :: !(Maybe City),
    addressCountry :: !(Maybe ISO3166Alpha2),
    addressLine1 :: !(Maybe AddressLine),
    addressLine2 :: !(Maybe AddressLine),
    addressDistrict :: !(Maybe District)
  }
  deriving (Eq, Show)

instance ToJSON Address where
  toJSON Address {..} =
    omitNulls
      [ "city" .= addressCity,
        "country" .= addressCountry,
        "line1" .= addressLine1,
        "line2" .= addressLine2,
        "district" .= addressDistrict
      ]

instance FromJSON Address where
  parseJSON = withObject "Address" parse
    where
      parse o =
        Address
          <$> o .: "city"
          <*> o .: "country"
          <*> o .: "line1"
          <*> o .: "line2"
          <*> o .: "district"

data BankAccountType = Wire | Sen deriving (Eq, Show)

instance ToJSON BankAccountType where
  toJSON Wire = String "wire"
  toJSON Sen = String "sen"

instance FromJSON BankAccountType where
  parseJSON (String s) = case T.unpack s of
    "wire" -> return Wire
    "sen" -> return Sen
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

data DestinationBankAccount = DestinationBankAccount
  { destinationBankAccountType :: !BankAccountType,
    destinationBankAccountId :: !UUID,
    destinationBankAccountName :: !(Maybe Text)
  }
  deriving (Eq, Show)

instance FromJSON DestinationBankAccount where
  parseJSON = withObject "DestinationBankAccount" parse
    where
      parse o =
        DestinationBankAccount
          <$> o .: "type"
          <*> o .: "id"
          <*> o .: "name"

instance ToJSON DestinationBankAccount where
  toJSON DestinationBankAccount {..} =
    omitNulls
      [ "type" .= destinationBankAccountType,
        "id" .= destinationBankAccountId,
        "name" .= destinationBankAccountName
      ]

-- TODO do we need type narrowing to have other types that represent subsets of this one without have to write
-- custom constructors?
data AllowedCurrencies = USD | EUR | BTC' | ETH' deriving (Eq, Show)

instance ToJSON AllowedCurrencies where
  toJSON USD = String "USD"
  toJSON EUR = String "EUR"
  toJSON BTC' = String "BTC"
  toJSON ETH' = String "ETH"

instance FromJSON AllowedCurrencies where
  parseJSON (String s) = case T.unpack s of
    "USD" -> return USD
    "EUR" -> return EUR
    "BTC" -> return BTC'
    "ETH" -> return ETH'
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

newtype Amount = Amount
  { unAmount :: Centi
  }
  deriving (Eq, Show, ToJSON, FromJSON)

-- TODO should I make more smart types like this to differentiate between different allowed amounts?  Seems like it's worth doing something like that,
-- given that this type actually lets you use any currency.
data USDOrEURAmount = USDOrEURAmount
  { usdOrEurAmount :: !Amount,
    usdOrEurCurrency :: !AllowedCurrencies
  }
  deriving (Eq, Show)

instance FromJSON USDOrEURAmount where
  parseJSON = withObject "USDOrEURAmount" parse
    where
      parse o =
        USDOrEURAmount
          <$> o .: "amount"
          <*> o .: "currency"

instance ToJSON USDOrEURAmount where
  toJSON USDOrEURAmount {..} =
    object
      [ "amount" .= usdOrEurAmount,
        "currency" .= usdOrEurCurrency
      ]

data USDAmount = USDAmount
  { usdAmountAmount :: !Amount,
    usdAmountCurrency :: !AllowedCurrencies
  }
  deriving (Eq, Show)

instance FromJSON USDAmount where
  parseJSON = withObject "USDAmount" parse
    where
      parse o =
        USDAmount
          <$> o .: "amount"
          <*> o .: "currency"

instance ToJSON USDAmount where
  toJSON USDAmount {..} =
    object
      [ "amount" .= usdAmountAmount,
        "currency" .= usdAmountCurrency
      ]

data Decision = Approved | Denied | Review deriving (Eq, Show)

instance ToJSON Decision where
  toJSON Approved = String "approved"
  toJSON Denied = String "denied"
  toJSON Review = String "review"

instance FromJSON Decision where
  parseJSON (String s) = case T.unpack s of
    "approved" -> return Approved
    "denied" -> return Denied
    "review" -> return Review
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

data RiskEvaluation = RiskEvaluation
  { riskEvaluationDecision :: !Decision,
    riskEvaluationReason :: !Text -- TODO probably fine, but maybe just give it a custom type to avoid too much text?
  }
  deriving (Eq, Show)

instance FromJSON RiskEvaluation where
  parseJSON = withObject "RiskEvaluation" parse
    where
      parse o =
        RiskEvaluation
          <$> o .: "amount"
          <*> o .: "currency"

data Adjustments = Adjustments
  { adjustmentsFXCredit :: !USDAmount,
    adjustmentsFXDebit :: !USDAmount
  }
  deriving (Show)

instance FromJSON Adjustments where
  parseJSON = withObject "Adjustments" parse
    where
      parse o =
        Adjustments
          <$> o .: "fxCredit"
          <*> o .: "fxDebit"

data BillingDetails = BillingDetails
  { billingDetailsName :: !Text,
    billingDetailsCity :: !City,
    billingDetailsCountry :: !ISO3166Alpha2,
    billingDetailsLine1 :: !AddressLine, -- address type
    billingDetailsLine2 :: !(Maybe AddressLine), -- secondary address type
    billingDetailsDistrict :: !(Maybe District), -- could be a state type
    billingDetailsPostalCode :: !PostalCode -- postal code type
  }
  deriving (Eq, Show)

instance ToJSON BillingDetails where
  toJSON BillingDetails {..} =
    omitNulls
      [ "name" .= billingDetailsName,
        "city" .= billingDetailsCity,
        "country" .= billingDetailsCountry,
        "line1" .= billingDetailsLine1,
        "line2" .= billingDetailsLine2,
        "district" .= billingDetailsDistrict,
        "postalCode" .= billingDetailsPostalCode
      ]

instance FromJSON BillingDetails where
  parseJSON = withObject "BillingDetails" parse
    where
      parse o =
        BillingDetails
          <$> o .: "name"
          <*> o .: "city"
          <*> o .: "country"
          <*> o .: "line1"
          <*> o .: "line2"
          <*> o .: "district"
          <*> o .: "postalCode"

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

instance ToJSON BankAddress where
  toJSON BankAddress {..} =
    omitNulls
      [ "name" .= bankAddressName,
        "city" .= bankAddressCity,
        "country" .= bankAddressCountry,
        "line1" .= bankAddressLine1,
        "line2" .= bankAddressLine2,
        "district" .= bankAddressDistrict
      ]

instance FromJSON BankAddress where
  parseJSON = withObject "BankAddress" parse
    where
      parse o =
        BankAddress
          <$> o .: "bankName"
          <*> o .: "city"
          <*> o .: "country"
          <*> o .: "line1"
          <*> o .: "line2"
          <*> o .: "district"

data BeneficiaryDetails = BeneficiaryDetails
  { beneficiaryDetailsName :: !Text,
    beneficiaryDetailsAddress1 :: !(Maybe AddressLine),
    beneficiaryDetailsAddress2 :: !(Maybe AddressLine)
  }
  deriving (Eq, Show)

instance FromJSON BeneficiaryDetails where
  parseJSON = withObject "BeneficiaryDetails" parse
    where
      parse o =
        BeneficiaryDetails
          <$> o .: "name"
          <*> o .: "address1"
          <*> o .: "address2"

data BeneficiaryBankDetails = BeneficiaryBankDetails
  { beneficiaryBankDetailsName :: !Text,
    beneficiaryBankDetailsSwiftCode :: !SwiftCode,
    beneficiaryBankDetailsRoutingNumber :: !RoutingNumber,
    beneficiaryBankDetailsAccountNumber :: !AccountNumber,
    beneficiaryBankDetailsCurrency :: !AllowedCurrencies,
    beneficiaryBankDetailsAddress :: !AddressLine,
    beneficiaryBankDetailsCity :: !City,
    beneficiaryBankDetailsPostalCode :: !PostalCode,
    beneficiaryBankDetailsCountry :: !Country
  }
  deriving (Eq, Show)

instance FromJSON BeneficiaryBankDetails where
  parseJSON = withObject "BeneficiaryBankDetails" parse
    where
      parse o =
        BeneficiaryBankDetails
          <$> o .: "name"
          <*> o .: "swiftCode"
          <*> o .: "routingNumber"
          <*> o .: "accountNumber"
          <*> o .: "currency"
          <*> o .: "address"
          <*> o .: "city"
          <*> o .: "postalCode"
          <*> o .: "country"

instance ToJSON BeneficiaryBankDetails where
  toJSON BeneficiaryBankDetails {..} =
    object
      [ "name" .= beneficiaryBankDetailsName,
        "swiftCode" .= beneficiaryBankDetailsSwiftCode,
        "routingNumber" .= beneficiaryBankDetailsRoutingNumber,
        "accountNumber" .= beneficiaryBankDetailsAccountNumber,
        "currency" .= beneficiaryBankDetailsCurrency,
        "address" .= beneficiaryBankDetailsAddress,
        "city" .= beneficiaryBankDetailsCity,
        "postalCode" .= beneficiaryBankDetailsPostalCode,
        "country" .= beneficiaryBankDetailsCountry
      ]

newtype AddressLine = AddressLine
  { unAddressLine :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

-- See 'accountNumberRegex' for where these numeric constraints come from
--
-- We're using greater than, less than without equality assertions in order
-- to better support 'Refined.weaken'
type AccountNumberConstraints =
  SizeGreaterThan 3
    && SizeLessThan 18

newtype AccountNumber = AccountNumber {unAccountNumber :: Refined AccountNumberConstraints Text}
  deriving stock (Eq, Show, Lift)
  deriving newtype (ToJSON)

-- Account numbers can have capital letters or digits
accountNumberRegex :: Regex
accountNumberRegex = [re|^[A-Z0-9]{4,17}$|]

mkAccountNumber :: Text -> Maybe AccountNumber
mkAccountNumber t =
  if t =~ accountNumberRegex
    then AccountNumber <$> refineFail t
    else Nothing

accountNumberToText :: AccountNumber -> Text
accountNumberToText (AccountNumber t) = unrefine t

type AccountNumberMask = Refined (SizeEqualTo 4) Text

accountNumberLastFour :: AccountNumber -> AccountNumberMask
accountNumberLastFour (AccountNumber n) =
  reallyUnsafeRefine
    . T.takeEnd 4
    $ unrefine n

accountNumberToByteString :: AccountNumber -> BS8.ByteString
accountNumberToByteString accountNumber = TE.encodeUtf8 $ accountNumberToText accountNumber

instance FromJSON AccountNumber where
  parseJSON = withText "AccountNumber" $ \t ->
    case mkAccountNumber t of
      Nothing -> fail $ "Invalid AccountNumber: " ++ T.unpack t
      Just accountNumber -> pure accountNumber

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

newtype City = City
  { unCity :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

-- TODO add constraints maybe?  Risk here is that I block valid codes.
newtype PostalCode = PostalCode
  { unPostalCode :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype SwiftCode = SwiftCode Text
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

-- TODO add constraints (should district be state?  Can I just use an enum?)
-- District will be a state only if it's US or Canada... will need to be clever here I think.
-- Maybe can have those two fields be modeled with a sum type?
newtype District = District
  { unDistrict :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype ISO3166Alpha2 = ISO3166Alpha2 Country
  deriving newtype (Eq, Enum, Ord)

instance ToJSON ISO3166Alpha2 where
  toJSON (ISO3166Alpha2 c) = String (alphaTwoUpper c)

instance FromJSON ISO3166Alpha2 where
  parseJSON = fmap ISO3166Alpha2 . parseJSON

instance Show ISO3166Alpha2 where
  show (ISO3166Alpha2 c) = T.unpack (alphaTwoUpper c)

-- TODO maybe constrain this?  Not sure what it looks like yet so that can happen later
newtype TrackingReference = TrackingReference
  { unTrackingReference :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

-- TODO maybe add validation?  I don't know enough about it yet.
newtype HexString = HexString
  { unHexString :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

-- TODO add validation if necessary
newtype CircleId = CircleId
  { unCircleId :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

-- TODO add validation if necessary
newtype WalletId = WalletId
  { unWalletId :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

-- TODO, consider replacing this hand-rolled validation with the Data.UUID library if you really care about accurate UUIDs.
-- This is fine for now, though.
newtype UUID = UUID
  { unUUID :: Text
  }
  deriving (Eq, Show)
  deriving newtype (ToJSON)

uuidRegex :: Regex
uuidRegex = [re|^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$|]

mkUUID :: Text -> Maybe UUID
mkUUID t =
  if t =~ uuidRegex
    then Just (UUID t)
    else Nothing

instance FromJSON UUID where
  parseJSON = withText "UUID" $ \t ->
    case mkUUID t of
      Nothing -> fail $ "Invalid UUID: " ++ T.unpack t
      Just uuid -> pure uuid

---------------------------------------------------------------
-- Utils
---------------------------------------------------------------

omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull (_, Array a) = (not . V.null) a
    notNull _ = True

utcToCircle :: UTCTime -> Text
utcToCircle ut =
  tshow day <> "T" <> clockTime <> "-00:00"
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show
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

-- | Similar to 'Either' but with different 'ToJSON' and 'FromJSON' instances.
-- 'ToJSON' will serialize the payload without adding any kind of tag.
-- 'FromJSON' will first attempt to parse JSON as the first type parameter,
-- and if that fails will then attempt to parse as the second type parameter.
--
-- __Engineer Note:__ The order of type parameters make a huge difference!
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
  deriving stock (Eq)

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
-- Payments API
-- this could probably be a new module
---------------------------------------------------------------

---------------------------------------------------------------
-- Payments endpoints
---------------------------------------------------------------

data PaymentRequest

type instance CircleRequest PaymentRequest = CircleResponseBody (ThisOrThat FiatPayment CryptoPayment)

data PaymentsRequest

-- this motherfucker will be a heterogenous list so I have have to figure out how to work with that in Haskell.  Should be a good learning exercise.
-- type instance CircleRequest PaymentsRequest = CircleResponseBody [FiatPayment, CryptoPayment, FiatCancel, FiatRefund]

type instance CircleRequest PaymentsRequest = CircleResponseBody [CryptoPayment]

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

data CryptoPayment = CryptoPayment
  { cryptoPaymentId :: !CircleId,
    cryptoPaymentType :: !PaymentType,
    cryptoPaymentMerchantId :: !CircleId, -- TODO maybe newtype for this
    cryptoPaymentMerchantWalletId :: !WalletId,
    cryptoPaymentMerchantAmount :: !USDAmount,
    cryptoPaymentStatus :: !PaymentStatus,
    cryptoPaymentFees :: !(Maybe USDAmount),
    cryptoPaymentPaymentIntentId :: !(Maybe UUID),
    cryptoPaymentSettlementAmount :: !(Maybe USDAmount), -- TODO this will probably change to not use Centi
    cryptoPaymentDepositAddress :: !(Maybe PaymentDepositAddress),
    cryptoPaymentTransactionHash :: !(Maybe Text), -- TODO this is probably a HexString too
    cryptoPaymentCreateDate :: !(Maybe UTCTime),
    cryptoPaymentUpdateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

instance FromJSON CryptoPayment where
  parseJSON = withObject "CryptoPayment" parse
    where
      parse o =
        CryptoPayment
          <$> o .: "id"
          <*> o .: "type"
          <*> o .: "merchantId"
          <*> o .: "merchantWalletId"
          <*> o .: "amount"
          <*> o .: "status"
          <*> o .:? "fees"
          <*> o .:? "paymentIntentId"
          <*> o .:? "settlementAmount"
          <*> o .:? "depositAddress"
          <*> o .:? "transactionHash"
          <*> o .:? "createDate"
          <*> o .:? "updateDate"

data PaymentDepositAddress = PaymentDepositAddress
  { paymentDepositAddressChain :: !Chain,
    paymentDepositAddressAddress :: !Text -- TODO this may be a hex string
  }
  deriving (Eq, Show)

instance FromJSON PaymentDepositAddress where
  parseJSON = withObject "PaymentDepositAddress" parse
    where
      parse o =
        PaymentDepositAddress
          <$> o .: "chain"
          <*> o .: "address"

-- {
--   "data": {
--     "id": "81855279-b53d-4119-9f1e-5d0af00f0c24",
--     "type": "payment",
--     "merchantId": "ff71551d-ae18-492d-baf1-d9205e20e0bf",
--     "merchantWalletId": "1000002584",
--     "amount": {
--       "amount": "5.00",
--       "currency": "USD"
--     },
--     "source": {
--       "id": "1fa990d9-fd12-400c-bc7d-e54a428f7570",
--       "type": "card"
--     },
--     "description": "Payment",
--     "createDate": 1583830582515,
--     "trackingRef": "20674453824672941243272",
--     "status": "confirmed",
--     "fees": {
--       "amount": "0.10",
--       "currency": "USD"
--     }
--   }
-- }

data FiatPayment = FiatPayment
  { fiatPaymentId :: !UUID,
    fiatPaymentType :: !PaymentType,
    fiatPaymentMerchantId :: !UUID, -- TODO maybe newtype for this
    fiatPaymentMerchantWalletId :: !WalletId,
    fiatPaymentMerchantAmount :: !USDAmount,
    fiatPaymentSource :: !PaymentSource,
    fiatPaymentDescription :: !(Maybe Text), -- TODO this should be an enum
    fiatPaymentStatus :: !PaymentStatus,
    fiatPaymentCaptured :: !Bool,
    fiatPaymentCaptureAmount :: !(Maybe USDAmount),
    fiatPaymentCaptureDate :: !(Maybe UTCTime),
    fiatPaymentRequiredAction :: !(Maybe PaymentActionRequired),
    fiatPaymentCancel :: !(Maybe FiatCancel),
    fiatPaymentRefunds :: !(Maybe [FiatRefund]),
    fiatPaymentFees :: !(Maybe USDAmount),
    fiatPaymentChannel :: !(Maybe Text), -- TODO this needs a type
    fiatPaymentCreateDate :: !(Maybe UTCTime),
    fiatPaymentUpdateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

instance FromJSON FiatPayment where
  parseJSON = withObject "FiatPayment" parse
    where
      parse o =
        FiatPayment
          <$> o .: "id"
          <*> o .: "type"
          <*> o .: "merchantId"
          <*> o .: "merchantWalletId"
          <*> o .: "amount"
          <*> o .: "source"
          <*> o .:? "description"
          <*> o .: "status"
          <*> o .: "captured"
          <*> o .:? "captureAmount"
          <*> o .:? "captureDate"
          <*> o .:? "requiredAction"
          <*> o .:? "cancel"
          <*> o .:? "refunds"
          <*> o .:? "fees"
          <*> o .:? "channel"
          <*> o .:? "createDate"
          <*> o .:? "updateDate"

data FiatRefund = FiatRefund
  { fiatRefundId :: !(Maybe CircleId),
    fiatRefundType :: !(Maybe PaymentType),
    fiatRefundAmount :: !(Maybe USDAmount),
    fiatRefundDescription :: !(Maybe Text), -- TODO description enum
    fiatRefundStatus :: !(Maybe PaymentStatus),
    fiatRefundRequiredAction :: !(Maybe PaymentActionRequired),
    fiatRefundFees :: !(Maybe USDAmount),
    fiatRefundCreateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

instance FromJSON FiatRefund where
  parseJSON = withObject "FiatRefund" parse
    where
      parse o =
        FiatRefund
          <$> o .:? "id"
          <*> o .:? "type"
          <*> o .:? "amount"
          <*> o .:? "description"
          <*> o .:? "status"
          <*> o .:? "requiredAction"
          <*> o .:? "fees"
          <*> o .:? "createDate"


data PaymentSource = PaymentSource
  { paymentSourceId :: !UUID,
    paymentSourceType :: !PaymentSourceType 
  }
  deriving (Eq, Show)

instance FromJSON PaymentSource where
  parseJSON = withObject "PaymentSource" parse
    where
      parse o =
        PaymentSource
          <$> o .: "id"
          <*> o .: "type"

data PaymentActionRequired = PaymentActionRequired
  { paymentActionRequiredType :: !ActionRequiredType,
    paymentActionRequiredRedirectUrl :: !Text -- TODO URL type
  }
  deriving (Eq, Show)

instance FromJSON PaymentActionRequired where
  parseJSON = withObject "PaymentActionRequired" parse
    where
      parse o =
        PaymentActionRequired
          <$> o .: "type"
          <*> o .: "redirectUrl"

data FiatCancel = FiatCancel
  { fiatCancelId :: !CircleId,
    fiatCancelType :: !PaymentType,
    fiatCancelDescription :: !Text, -- TODO description enum
    fiatCancelStatus :: !PaymentStatus,
    fiatCancelCreateDate :: !UTCTime
  }
  deriving (Eq, Show)

instance FromJSON FiatCancel where
  parseJSON = withObject "FiatCancel" parse
    where
      parse o =
        FiatCancel
          <$> o .: "id"
          <*> o .: "type"
          <*> o .: "description"
          <*> o .: "status"
          <*> o .: "createDate"

data ActionRequiredType = ThreeDSecureRequired deriving (Eq, Show)

instance FromJSON ActionRequiredType where
  parseJSON (String s) = case T.unpack s of
    "three_d_secure_required" -> return ThreeDSecureRequired
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

data PaymentType = Payment | Cancel | Refund deriving (Eq, Show)

instance FromJSON PaymentType where
  parseJSON (String s) = case T.unpack s of
    "payment" -> return Payment
    "cancel" -> return Cancel
    "refund" -> return Refund
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

data PaymentSourceType = Card | ACH | WireSource | SEPA deriving (Eq, Show)

instance FromJSON PaymentSourceType where
  parseJSON (String s) = case T.unpack s of
    "card" -> return Card
    "ach" -> return ACH
    "wire" -> return WireSource
    "sepa" -> return SEPA
    _ -> error "JSON format not expected"
  parseJSON _ = error "JSON format not expected"

-- TODO: the subsequent two types look exactly the same but one is of type Cancel and the other is of type Refund.  How to do this in Haskell?