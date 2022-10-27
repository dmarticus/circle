{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Unknot.Types
  ( -- Types for connecting to and wrapping Circle's API
    ApiToken (..),
    Reply,
    Method,
    CircleAPIRequest (..),
    CircleError (..),
    CircleResponse (..),
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
    -- Shared types across different endpoints
    DestinationBankAccount (..),
    USDOrEURAmount (..),
    -- PayoutQueryParams (..),
    BankAccountType (..),
    AllowedCurrencies (..),
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
    ToJSON (toJSON, toEncoding),
    Value (Array, Null, String),
    object,
    withObject,
    withText,
    (.:),
    (.:?), Result (Success, Error)
  )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Coerce (coerce)
import Data.Fixed (Centi, Fixed (MkFixed))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
import Data.Bifunctor
import Data.Aeson.Types (fromJSON)
import Data.Foldable

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
    -- TODO feels like I need some sort of way to differentiate between body and query params, hmm
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
  { parseError :: !String,
    circleResponse :: !(Response BSL.ByteString)
  }
  deriving (Show)

data CircleResponse a = CircleResponse
  { circleResponseCode :: !(Maybe ResponseStatus),
    circleResponseMessage :: !(Maybe ResponseMessage),
    circleResponseData :: !(Maybe a)
  }
  deriving (Eq, Show)

instance FromJSON a => FromJSON (CircleResponse a) where
  parseJSON = withObject "CircleResponse" parse
    where
      parse o =
        CircleResponse
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

type Reply = Network.HTTP.Client.Response BSL.ByteString

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

data PaginationQueryParam = PageBefore !Text | PageAfter !Text deriving (Show, Eq) -- TODO these will be IDs, but not UUIDs.  No type until we figure out what they are.

instance ToCircleParam PaginationQueryParams where
  toCircleParam (PaginationQueryParams p) =
    -- Circle has some BS pagination where they let users supply some canonical
    -- collection ID, and then this pagination rule will return `n` entries before OR after that page,
    -- where `n` is controlled by the pageSize param.  This type exists to prevent callers from providing both params, which would error out
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

---------------------------------------------------------------
-- Balance endpoints
---------------------------------------------------------------

data BalanceRequest

type instance CircleRequest BalanceRequest = CircleResponse BalanceData

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

---------------------------------------------------------------
-- Payout endpoints
---------------------------------------------------------------
data PayoutRequest

type instance CircleRequest PayoutRequest = CircleResponse PayoutData

data PayoutsRequest

type instance CircleRequest PayoutsRequest = CircleResponse [PayoutData]

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
  { payoutDataId :: !UUID,
    payoutDataSourceWalletId :: !UUID,
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
          <*> o .: "payoutPayoutErrorCode"
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

type instance CircleRequest ConfigurationRequest = CircleResponse ConfigurationData

newtype ConfigurationData = ConfigurationData {configurationDataPayments :: WalletConfig}
  deriving (Eq, Show)

instance FromJSON ConfigurationData where
  parseJSON = withObject "ConfigurationData" parse
    where
      parse o =
        ConfigurationData
          <$> o .: "payments"

newtype WalletConfig = WalletConfig {masterWalletId :: Text}
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

type instance CircleRequest EncryptionRequest = CircleResponse EncryptionData

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

type instance CircleRequest ChannelsRequest = CircleResponse ChannelData

newtype ChannelData = ChannelData {channels :: [Channel]}
  deriving (Eq, Show)

instance FromJSON ChannelData where
  parseJSON = withObject "ChannelData" parse
    where
      parse o =
        ChannelData
          <$> o .: "channels"

data Channel = Channel
  { channelId :: !Text,
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

type instance CircleRequest StablecoinsRequest = CircleResponse [StablecoinData]

data StablecoinData = StablecoinData
  { stablecoinDataName :: !Text,
    stablecoinDataSymbol :: !Text, -- TODO enum of stablecoin symbols
    stablecoinDataTotalAmount :: !Text, -- TODO should this be Amount?  probably
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

---------------------------------------------------------------
-- Subscription endpoints
---------------------------------------------------------------

data SubscriptionsRequest

type instance CircleRequest SubscriptionsRequest = CircleResponse [SubscriptionData]

data SubscriptionRequest

type instance CircleRequest SubscriptionRequest = CircleResponse SubscriptionData

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

type instance CircleRequest TransfersRequest = CircleResponse [TransferData]

instance CircleHasParam TransfersRequest PaginationQueryParams
instance CircleHasParam TransfersRequest FromQueryParam
instance CircleHasParam TransfersRequest ToQueryParam
instance CircleHasParam TransfersRequest PageSizeQueryParam

data TransferData = TransferData 
  { transferDataId :: !Text, -- TODO some sort of internal ID that's not a UUID
    transferDataSource :: !(ThisOrThat SourceWallet SourceBlockchain),
    transferDataDestination :: !(ThisOrThat DestinationWallet DestinationBlockchain),
    transferDataAmount :: !CurrencyAmount,
    transferDataFees :: !(Maybe USDAmount),
    transferDataTransactionHash :: !(Maybe Text), -- TODO there's probably a type here
    transferDataStatus :: !Status,
    transferDataTransferErrorCode :: !(Maybe TransferErrorCode),
    transferDataCreateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

data SourceWallet = SourceWallet
  { sourceWalletType :: !Text, -- it's just gonna be "wallet"
    sourceWalletId :: !Text, -- TODO better type
    sourceWalletIdentities :: ![Identity]
  }
  deriving (Eq, Show)

data SourceBlockchain = SourceBlockchain
  { sourceBlockchainType :: !Text, -- just "blockchain"
    sourceBlockchainChain :: !Chain,
    sourceBlockChainIdentities :: ![Identity]
  } deriving (Eq, Show)

data DestinationWallet = DestinationWallet
  { destinationWalletType :: !Text, -- just "wallet"
    destinationWalletId :: !Text,
    destinationWalletAddress :: !(Maybe Text),  -- TODO alphanum ID
    destinationWalletAddressTag :: !(Maybe Text)
  }
  deriving (Eq, Show)

data DestinationBlockchain = DestinationBlockchain
  { destinationBlockchainType :: !Text, -- just "blockchain"
    destinationBlockchainAddress :: !Text,
    destinationBlockchainAddressTag :: !(Maybe Text), -- TODO alphanum ID
    destinationBlockchainAddressChain :: !Chain
  }
  deriving (Eq, Show)

data Identity = Identity
  { identityType :: !IdentityType,
    identityName :: !Text,
    identityAddresses :: ![Address]
  }
  deriving (Eq, Show)

data IdentityType = Individual | Business deriving (Eq, Show)
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
-- Wire endpoints
---------------------------------------------------------------

data WireAccountRequest

type instance CircleRequest WireAccountRequest = CircleResponse WireAccountData

data WireAccountsRequest

type instance CircleRequest WireAccountsRequest = CircleResponse [WireAccountData]

data WireInstructionsRequest

type instance CircleRequest WireInstructionsRequest = CircleResponse WireInstructionsData

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
    usdAmountFeeCurrency :: !AllowedCurrencies
  }
  deriving (Eq, Show)

instance FromJSON USDAmount where
  parseJSON = withObject "USDAmount" parse
    where
      parse o =
        USDAmount
          <$> o .: "amount"
          <*> o .: "currency"

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
      quotePat = error "RoutingNumber is not a patttern; use routingNumberToText instead",
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
            [ "Failed when parsing a ThisOrThat from JSON.\n"
            , "Error on the This: " <> thisError <> "\n"
            , "Error on the That: " <> thatError
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