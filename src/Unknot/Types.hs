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
    USBankAccountBodyParams (..),
    IBANBankAccountBodyParams (..),
    NonIBANBankAccountBodyParams (..),
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
    CircleId (..),
    MoneyAmount (..),
    BankAccountType (..),
    SupportedCurrencies (..),
    Chain (..),
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
    compileUUID,
    TrackingReference (..),
    catThats,
    catThises,
    thisOrThat,
    thisOrThatToEither,
    ThisOrThat (..),
    -- Payments API --
    -- Payments endpoints
    PaymentRequest,
    PaymentsRequest,
    CreatePaymentBody (..),
    -- CryptoPayment (..),
    FiatOrCryptoPaymentResponse (..),
    CreateMetadata (..),
    PaymentErrorCode (..),
    Metadata (..),
    FiatCancelOrRefund (..),
    -- FiatCancel (..),
    VerificationType (..),
    PaymentSource (..),
    PaymentSourceType (..),
    CancelPaymentBody (..),
    CancelPaymentReason (..),
    RefundPaymentBody (..),
    MockPaymentRequest,
    MockSenOrWirePaymentBodyParams (..),
    MockBeneficiaryBankDetails (..),
    MockSEPAPaymentBodyParams (..),
    MockPaymentData (..),
    -- On-chain Payments Endpoint
    OnChainTransferRequest,
    OnChainTransfersRequest,
    OnChainTransferBodyParams (..),
    -- Cards Endpoint
    CardsRequest,
    CardRequest,
    CardData (..),
    ListCardData (..),
    CreateCardBodyParams (..),
    UpdateCardBodyParams (..),
    -- ACH Endpoint
    ACHBankAccountRequest,
    ACHBankAccountData (..),
    -- SEPA Endpoint
    SEPAAccountData (..),
    SEPAAccountRequest,
    SEPAAccountBodyParams (..),
    SEPAInstructionsRequest,
    -- Settlements Endpoint
    SettlementsRequest,
    SettlementRequest,
  )
where

import Autodocodec
  ( Autodocodec (Autodocodec),
    HasCodec (codec),
    JSONCodec,
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
import Country.Identifier (americanSamoa, guam, northernMarianaIslands, puertoRico, unitedStatesMinorOutlyingIslands, unitedStatesOfAmerica, virginIslandsUs)
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
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.Validity
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

-- NB: This type parses every response from the Circle API, so I can't use Autodocodec to derive FromJSON here
-- because I'm using ThisOrThat (see below) as a smart parser for certain types, and that type doesn't have Autodocodec instances
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
-- Balance endpoints
---------------------------------------------------------------

data BalanceRequest

type instance CircleRequest BalanceRequest = CircleResponseBody BalanceData

data BalanceData = BalanceData
  { balanceDataAvailable :: ![MoneyAmount],
    balanceDataUnsettled :: ![MoneyAmount]
  }
  deriving (Eq, Show, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec BalanceData)

instance HasCodec BalanceData where
  codec =
    object "BalanceData" $
      BalanceData
        <$> requiredField' "available" .= balanceDataAvailable
        <*> requiredField' "unsettled" .= balanceDataAvailable

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
    payoutReturnAmount :: !MoneyAmount,
    payoutReturnFees :: !MoneyAmount,
    payoutReturnReason :: !Text,
    payoutReturnStatus :: !Status,
    payoutReturnCreateDate :: !UTCTime,
    payoutReturnUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec PayoutReturn)

instance HasCodec PayoutReturn where
  codec =
    object "PayoutReturn" $
      PayoutReturn
        <$> requiredField' "id" .= payoutReturnId
        <*> requiredField' "payoutId" .= payoutReturnOriginalPayoutId
        <*> requiredField' "amount" .= payoutReturnAmount
        <*> requiredField' "fees" .= payoutReturnFees
        <*> requiredField' "reason" .= payoutReturnReason
        <*> requiredField' "status" .= payoutReturnStatus
        <*> requiredField' "createDate" .= payoutReturnCreateDate
        <*> requiredField' "updateDate" .= payoutReturnUpdateDate

data PayoutData = PayoutData
  { payoutDataId :: !CircleId,
    payoutDataSourceWalletId :: !WalletId,
    payoutDataDestinationBankAccount :: !DestinationBankAccount,
    payoutDataAmount :: !MoneyAmount,
    payoutDataFees :: !MoneyAmount,
    payoutDataStatus :: !Status,
    payoutDataTrackingRef :: !TrackingReference, -- TODO maybe this needs a custom type, although text is probably fine
    payoutDataPayoutErrorCode :: !PayoutErrorCode,
    payoutDataRiskEvaluation :: !RiskEvaluation,
    payoutDataAdjustments :: !Adjustments,
    payoutDataPayoutReturn :: !PayoutReturn,
    payoutDataCreateDate :: !UTCTime,
    payoutDataUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec PayoutData)

instance HasCodec PayoutData where
  codec =
    object "PayoutData" $
      PayoutData
        <$> requiredField' "id" .= payoutDataId
        <*> requiredField' "sourceWalletId" .= payoutDataSourceWalletId
        <*> requiredField' "destination" .= payoutDataDestinationBankAccount
        <*> requiredField' "amount" .= payoutDataAmount
        <*> requiredField' "fees" .= payoutDataFees
        <*> requiredField' "status" .= payoutDataStatus
        <*> requiredField' "trackingRef" .= payoutDataTrackingRef
        <*> requiredField' "errorCode" .= payoutDataPayoutErrorCode
        <*> requiredField' "riskEvaluation" .= payoutDataRiskEvaluation
        <*> requiredField' "adjustments" .= payoutDataAdjustments
        <*> requiredField' "payoutReturn" .= payoutDataPayoutReturn
        <*> requiredField' "createDate" .= payoutDataCreateDate
        <*> requiredField' "updateDate" .= payoutDataUpdateDate

data PayoutBodyParams = PayoutBodyParams
  { payoutBodyParamsIdempotencyKey :: !UUID,
    payoutBodyParamsDestination :: !DestinationBankAccount,
    payoutBodyParamsAmount :: !MoneyAmount
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec PayoutBodyParams)

instance HasCodec PayoutBodyParams where
  codec =
    object "PayoutBodyParams" $
      PayoutBodyParams
        <$> requiredField' "idempotencyKey" .= payoutBodyParamsIdempotencyKey
        <*> requiredField' "destination" .= payoutBodyParamsDestination
        <*> requiredField' "amount" .= payoutBodyParamsAmount

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
-- Management endpoint
---------------------------------------------------------------

data ConfigurationRequest

type instance CircleRequest ConfigurationRequest = CircleResponseBody ConfigurationData

newtype ConfigurationData = ConfigurationData {configurationDataPayments :: WalletConfig}
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ConfigurationData)

instance HasCodec ConfigurationData where
  codec =
    object "ConfigurationData" $
      ConfigurationData
        <$> requiredField' "payments" .= configurationDataPayments

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
-- Encryption endpoint
---------------------------------------------------------------

data EncryptionRequest

type instance CircleRequest EncryptionRequest = CircleResponseBody EncryptionData

data EncryptionData = EncryptionData
  { encryptionDataKeyId :: !Text, -- TODO this should actually be a UUID, but for the tests to work it needs to be relaxed a bit
    encryptionDataPublicKey :: !Text -- TODO is there a PGP key type in Haskell?
  }
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON) via (Autodocodec EncryptionData)

instance HasCodec EncryptionData where
  codec =
    object "EncryptionData" $
      EncryptionData
        <$> requiredField' "keyId" .= encryptionDataKeyId
        <*> requiredField' "publicKey" .= encryptionDataPublicKey

---------------------------------------------------------------
-- Channels endpoint
---------------------------------------------------------------

data ChannelsRequest

type instance CircleRequest ChannelsRequest = CircleResponseBody ChannelData

newtype ChannelData = ChannelData {channels :: [Channel]}
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ChannelData)

instance HasCodec ChannelData where
  codec =
    object "ChannelData" $
      ChannelData
        <$> requiredField' "channels" .= channels

data Channel = Channel
  { channelId :: !CircleId,
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
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec StablecoinData)

instance HasCodec StablecoinData where
  codec =
    object "StablecoinData" $
      StablecoinData
        <$> requiredField' "name" .= stablecoinDataName
        <*> requiredField' "symbol" .= stablecoinDataSymbol
        <*> requiredField' "totalAmount" .= stablecoinDataTotalAmount
        <*> requiredField' "chains" .= stablecoinDataChains

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

data Chain = ALGO | ARB | AVAX | ChainBTC | ChainETH | FLOW | HBAR | MATIC | SOL | TRX | XLM
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
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SubscriptionData)

instance HasCodec SubscriptionData where
  codec =
    object "SubscriptionData" $
      SubscriptionData
        <$> requiredField' "id" .= subscriptionDataId
        <*> requiredField' "endpoint" .= subscriptionDataEndpoint
        <*> requiredField' "subscriptionDetails" .= subscriptionDataSubscriptionDetails

data SubscriptionDetails = SubscriptionDetails
  { subscriptionDetailsUrl :: !Text,
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

newtype SubscriptionBodyParams = SubscriptionBodyParams
  { subscriptionBodyParamsEndpoint :: Text
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SubscriptionBodyParams)

instance HasCodec SubscriptionBodyParams where
  codec =
    object "SubscriptionBodyParams" $
      SubscriptionBodyParams
        <$> requiredField' "endpoint" .= subscriptionBodyParamsEndpoint

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
  { transferBodyParamsIdempotencyKey :: !UUID,
    transferBodyParamsDestination :: !TransferBodyDestination,
    transferBodyParamsAmount :: !MoneyAmount
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec TransferBodyParams)

instance HasCodec TransferBodyParams where
  codec =
    object "TransferBodyParams" $
      TransferBodyParams
        <$> requiredField' "idempotencyKey" .= transferBodyParamsIdempotencyKey
        <*> requiredField' "destination" .= transferBodyParamsDestination
        <*> requiredField' "amount" .= transferBodyParamsAmount

data TransferBodyDestination = TransferBodyDestination
  { transferBodyDestinationType :: !DestinationType,
    transferBodyDestinationAddressId :: !UUID
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec TransferBodyDestination)

instance HasCodec TransferBodyDestination where
  codec =
    object "TransferBodyDestination" $
      TransferBodyDestination
        <$> requiredField' "type" .= transferBodyDestinationType
        <*> requiredField' "addressId" .= transferBodyDestinationAddressId

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

data TransferData = TransferData
  { transferDataId :: !CircleId,
    transferDataSource :: !(ThisOrThat SourceWallet SourceBlockchain),
    transferDataDestination :: !(ThisOrThat DestinationWallet DestinationBlockchain),
    transferDataAmount :: !MoneyAmount,
    transferDataFees :: !TransferFeeAmount,
    transferDataTransactionHash :: !(Maybe HexString),
    transferDataStatus :: !Status,
    transferDataTransferErrorCode :: !(Maybe TransferErrorCode),
    transferDataRiskEvaluation :: !(Maybe RiskEvaluation),
    transferDataCreateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

-- NB: this doesn't use autodocodec for deriving ToJSON and FromJSON since I'm using the hand-rolled
-- ThisOrThat helper for smartly parsing types.
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
          <*> o .:? "transactionHash"
          <*> o .: "status"
          <*> o .:? "errorCode"
          <*> o .:? "riskEvaluation"
          <*> o .:? "createDate"

data SourceWallet = SourceWallet
  { sourceWalletType :: !TransferType,
    sourceWalletId :: !WalletId, -- From Circle's docs: "Numeric value but should be treated as a string as format may change in the future"
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
-- Address endpoints
---------------------------------------------------------------

data DepositAddressesRequest

type instance CircleRequest DepositAddressesRequest = CircleResponseBody [DepositAddressData]

data DepositAddressRequest

type instance CircleRequest DepositAddressRequest = CircleResponseBody DepositAddressData

data DepositAddressData = DepositAddressData
  { depositAddressAddress :: !HexString,
    depositAddressAddressTag :: !(Maybe CircleId), -- The docs say it's on the response, but the sandbox API doesn't return in. Make it a `Maybe` for now.
    depositAddressCurrency :: !SupportedCurrencies,
    depositAddressChain :: !Chain
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec DepositAddressData)

instance HasCodec DepositAddressData where
  codec =
    object "DepositAddressData" $
      DepositAddressData
        <$> requiredField' "address" .= depositAddressAddress
        <*> optionalField' "addressTag" .= depositAddressAddressTag
        <*> requiredField' "currency" .= depositAddressCurrency
        <*> requiredField' "chain" .= depositAddressChain

data DepositAddressBodyParams = DepositAddressBodyParams
  { depositAddressBodyIdempotencyKey :: !UUID,
    depositAddressBodyCurrency :: !SupportedCurrencies,
    depositAddressBodyChain :: !Chain
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec DepositAddressBodyParams)

instance HasCodec DepositAddressBodyParams where
  codec =
    object "DepositAddressBodyParams" $
      DepositAddressBodyParams
        <$> requiredField' "idempotencyKey" .= depositAddressBodyIdempotencyKey
        <*> requiredField' "currency" .= depositAddressBodyCurrency
        <*> requiredField' "chain" .= depositAddressBodyChain

data RecipientAddressesRequest

type instance CircleRequest RecipientAddressesRequest = CircleResponseBody [RecipientAddressData]

instance CircleHasParam RecipientAddressesRequest PaginationQueryParams

instance CircleHasParam RecipientAddressesRequest FromQueryParam

instance CircleHasParam RecipientAddressesRequest ToQueryParam

instance CircleHasParam RecipientAddressesRequest PageSizeQueryParam

data RecipientAddressRequest

type instance CircleRequest RecipientAddressRequest = CircleResponseBody RecipientAddressData

data RecipientAddressData = RecipientAddressData
  { recipientAddressId :: !UUID,
    recipientAddressAddress :: !HexString,
    recipientAddressAddressTag :: !(Maybe CircleId),
    recipientAddressChain :: !Chain,
    recipientAddressCurrency :: !SupportedCurrencies,
    recipientAddressDescription :: !Text
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec RecipientAddressData)

instance HasCodec RecipientAddressData where
  codec =
    object "RecipientAddressData" $
      RecipientAddressData
        <$> requiredField' "id" .= recipientAddressId
        <*> requiredField' "address" .= recipientAddressAddress
        <*> optionalField' "addressTag" .= recipientAddressAddressTag
        <*> requiredField' "chain" .= recipientAddressChain
        <*> requiredField' "currency" .= recipientAddressCurrency
        <*> requiredField' "description" .= recipientAddressDescription

data RecipientAddressBodyParams = RecipientAddressBodyParams
  { recipientAddressBodyIdempotencyKey :: !UUID,
    recipientAddressBodyAddress :: !HexString,
    recipientAddressBodyAddressTag :: !(Maybe CircleId),
    recipientAddressBodyChain :: !Chain,
    recipientAddressBodyCurrency :: !SupportedCurrencies,
    recipientAddressBodyDescription :: !Text
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec RecipientAddressBodyParams)

instance HasCodec RecipientAddressBodyParams where
  codec =
    object "RecipientAddressBodyParams" $
      RecipientAddressBodyParams
        <$> requiredField' "idempotencyKey" .= recipientAddressBodyIdempotencyKey
        <*> requiredField' "address" .= recipientAddressBodyAddress
        <*> optionalField' "addressTag" .= recipientAddressBodyAddressTag
        <*> requiredField' "chain" .= recipientAddressBodyChain
        <*> requiredField' "currency" .= recipientAddressBodyCurrency
        <*> requiredField' "description" .= recipientAddressBodyDescription

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
    depositAmount :: !MoneyAmount,
    depositFee :: !MoneyAmount,
    depositStatus :: !Status,
    depositRiskEvaluation :: !(Maybe RiskEvaluation),
    depositCreateDate :: !UTCTime,
    depositUpdateDate :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec DepositData)

instance HasCodec DepositData where
  codec =
    object "DepositData" $
      DepositData
        <$> requiredField' "id" .= depositId
        <*> optionalField' "sourceWalletId" .= depositSourceWalletId
        <*> requiredField' "destination" .= depositDestination
        <*> requiredField' "amount" .= depositAmount
        <*> requiredField' "fee" .= depositFee
        <*> requiredField' "status" .= depositStatus
        <*> requiredField' "riskEvaluation" .= depositRiskEvaluation
        <*> requiredField' "createDate" .= depositCreateDate
        <*> requiredField' "updateDate" .= depositUpdateDate

-- TODO is this real?  Like should I make some real code that does what this mock does and expose that in the module?
data MockPaymentRequest

type instance CircleRequest MockPaymentRequest = CircleResponseBody MockPaymentData

-- TODO bidirectional types should have names
data MockPaymentData = MockPaymentData
  { mockPaymentDataTrackingRef :: !(Maybe TrackingReference),
    mockPaymentDataAmount :: !(Maybe MoneyAmount),
    mockPaymentDataBeneficiaryBank :: !(Maybe BeneficiaryBankDetails),
    mockPaymentDataStatus :: !(Maybe Status)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MockPaymentData)

instance HasCodec MockPaymentData where
  codec =
    object "MockPaymentData" $
      MockPaymentData
        <$> optionalField' "trackingRef" .= mockPaymentDataTrackingRef
        <*> optionalField' "amount" .= mockPaymentDataAmount
        <*> optionalField' "beneficiaryBank" .= mockPaymentDataBeneficiaryBank
        <*> optionalField' "status" .= mockPaymentDataStatus

data MockSenOrWirePaymentBodyParams = MockSenOrWirePaymentBodyParams
  { mockSenOrWirePaymentBodyParamsTrackingRef :: !TrackingReference,
    mockSenOrWirePaymentBodyParamsAmount :: !MoneyAmount,
    mockSenOrWirePaymentBodyParamsBeneficiaryBank :: !MockBeneficiaryBankDetails
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MockSenOrWirePaymentBodyParams)

instance HasCodec MockSenOrWirePaymentBodyParams where
  codec =
    object "MockSenOrWirePaymentBodyParams" $
      MockSenOrWirePaymentBodyParams
        <$> requiredField' "trackingRef" .= mockSenOrWirePaymentBodyParamsTrackingRef
        <*> requiredField' "amount" .= mockSenOrWirePaymentBodyParamsAmount
        <*> requiredField' "beneficiaryBank" .= mockSenOrWirePaymentBodyParamsBeneficiaryBank

data MockSEPAPaymentBodyParams = MockSEPAPaymentBodyParams
  { mockSEPAPaymentBodyParamsTrackingRef :: !TrackingReference,
    mockSEPAPaymentBodyParamsAmount :: !MoneyAmount
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MockSEPAPaymentBodyParams)

instance HasCodec MockSEPAPaymentBodyParams where
  codec =
    object "MockSEPAPaymentBodyParams" $
      MockSEPAPaymentBodyParams
        <$> requiredField' "trackingRef" .= mockSEPAPaymentBodyParamsTrackingRef
        <*> requiredField' "amount" .= mockSEPAPaymentBodyParamsAmount

newtype MockBeneficiaryBankDetails = MockBeneficiaryBankDetails {mockBeneficiaryBankDetailsAccountNumber :: AccountNumber}
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec MockBeneficiaryBankDetails where
  codec = dimapCodec MockBeneficiaryBankDetails mockBeneficiaryBankDetailsAccountNumber codec

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
    senAccountBodyParamsCurrency :: !(Maybe SupportedCurrencies)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SENAccountBodyParams)

instance HasCodec SENAccountBodyParams where
  codec =
    object "SENAccountBodyParams" $
      SENAccountBodyParams
        <$> requiredField' "idempotencyKey" .= senAccountBodyParamsIdempotencyKey
        <*> requiredField' "accountNumber" .= senAccountBodyParamsAccountNumber
        <*> optionalField' "currency" .= senAccountBodyParamsCurrency

data SENAccountData = SENAccountData
  { senAccountDataId :: !UUID,
    senAccountDataStatus :: !Status,
    senAccountDataDescription :: !Text, -- TODO better type: Bank name plus last four digits of the bank account number or IBAN.  Make a custom type for this
    senAccountDataTrackingRef :: !TrackingReference,
    senAccountDataCreateDate :: !UTCTime,
    senAccountDataUpdateDate :: !UTCTime,
    senAccountDataCurrency :: !(Maybe SupportedCurrencies)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SENAccountData)

instance HasCodec SENAccountData where
  codec =
    object "SENAccountData" $
      SENAccountData
        <$> requiredField' "id" .= senAccountDataId
        <*> requiredField' "status" .= senAccountDataStatus
        <*> requiredField' "description" .= senAccountDataDescription
        <*> requiredField' "trackingRef" .= senAccountDataTrackingRef
        <*> requiredField' "createDate" .= senAccountDataCreateDate
        <*> requiredField' "updateDate" .= senAccountDataUpdateDate
        <*> optionalField' "currency" .= senAccountDataCurrency

data SENInstructionsData = SENInstructionsData
  { senInstructionsDataTrackingRef :: !TrackingReference, -- TODO need to figure out how to have clean ways to write Autodocodec instances for newtypes that are already deriving ToJSON and FromJSON.
    senInstructionsDataAccountNumber :: !AccountNumber,
    senInstructionsDataCurrency :: !SupportedCurrencies
  }
  deriving stock (Eq, Show, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SENInstructionsData)

instance HasCodec SENInstructionsData where
  codec =
    object "SENInstructionsData" $
      SENInstructionsData
        <$> requiredField' "trackingRef" .= senInstructionsDataTrackingRef
        <*> requiredField' "accountNumber" .= senInstructionsDataAccountNumber
        <*> requiredField' "currency" .= senInstructionsDataCurrency

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
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SignetBankAccountBodyParams)

instance HasCodec SignetBankAccountBodyParams where
  codec =
    object "SignetBankAccountBodyParams" $
      SignetBankAccountBodyParams
        <$> requiredField' "idempotencyKey" .= signetBankAccountBodyParamsIdempotencyKey
        <*> requiredField' "walletAddress" .= signetBankAccountBodyParamsWalletAddress

data SignetBankAccountData = SignetBankAccountData
  { signetBankAccountId :: !CircleId,
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
    via (Autodocodec SignetBankAccountData)

instance HasCodec SignetBankAccountData where
  codec =
    object "SignetBankAccountData" $
      SignetBankAccountData
        <$> requiredField' "id" .= signetBankAccountId
        <*> requiredField' "status" .= signetBankAccountStatus
        <*> requiredField' "trackingRef" .= signetBankAccountTrackingRef
        <*> requiredField' "walletAddress" .= signetBankAccountWalletAddress
        <*> requiredField' "createDate" .= signetBankAccountCreateDate
        <*> requiredField' "updateDate" .= signetBankAccountUpdateDate

data SignetBankInstructionsData = SignetBankInstructionsData
  { signetBankInstructionsTrackingRef :: !(Maybe TrackingReference),
    signetBankInstructionsWalletAddress :: !(Maybe HexString) -- TODO this should have a type, looks like this 0xcac04f0069e4ac9314ac4e608e99278a3bebabcd
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SignetBankInstructionsData)

instance HasCodec SignetBankInstructionsData where
  codec =
    object "SignetBankInstructionsData" $
      SignetBankInstructionsData
        <$> optionalField' "trackingRef" .= signetBankInstructionsTrackingRef
        <*> optionalField' "walletAddress" .= signetBankInstructionsWalletAddress

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

data WireAccountBodyParams
  = USBankAccount !USBankAccountBodyParams
  | IBANBankAccount !IBANBankAccountBodyParams
  | NonIBANBankAccount !NonIBANBankAccountBodyParams
  deriving (Eq, Show)

data USBankAccountBodyParams = USBankAccountBodyParams
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
    via (Autodocodec USBankAccountBodyParams)

instance HasCodec USBankAccountBodyParams where
  codec =
    object "USBankAccountBodyParams" $
      USBankAccountBodyParams
        <$> requiredField' "idempotencyKey" .= usBankAccountIdempotencyKey
        <*> requiredField' "accountNumber" .= usBankAccountAccountNumber
        <*> requiredField' "routingNumber" .= usBankAccountRoutingNumber
        <*> requiredField' "billingDetails" .= usBankAccountBillingDetails
        <*> requiredField' "bankAddress" .= usBankAccountBankAddress

data IBANBankAccountBodyParams = IBANBankAccountBodyParams
  { ibanBankAccountIdempotencyKey :: !UUID,
    ibanBankAccountIBAN :: !Text, -- TODO needs IBAN newtype
    ibanBankAccountBillingDetails :: !BillingDetails,
    ibanBankAccountBankAddress :: !BankAddress
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec IBANBankAccountBodyParams)

instance HasCodec IBANBankAccountBodyParams where
  codec =
    object "IBANBankAccountBodyParams" $
      IBANBankAccountBodyParams
        <$> requiredField' "idempotencyKey" .= ibanBankAccountIdempotencyKey
        <*> requiredField' "iban" .= ibanBankAccountIBAN
        <*> requiredField' "billingDetails" .= ibanBankAccountBillingDetails
        <*> requiredField' "bankAddress" .= ibanBankAccountBankAddress

data NonIBANBankAccountBodyParams = NonIBANBankAccountBodyParams
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
    via (Autodocodec NonIBANBankAccountBodyParams)

instance HasCodec NonIBANBankAccountBodyParams where
  codec =
    object "NonIBANBankAccountBodyParams" $
      NonIBANBankAccountBodyParams
        <$> requiredField' "idempotencyKey" .= nonIBANBankAccountIdempotencyKey
        <*> requiredField' "accountNumber" .= nonIBANBankAccountAccountNumber
        <*> requiredField' "routingNumber" .= nonIBANBankAccountRoutingNumber
        <*> requiredField' "billingDetails" .= nonIBANBankAccountBillingDetails
        <*> requiredField' "bankAddress" .= nonIBANBankAccountBankAddress

data WireInstructionsData = WireInstructionsData
  { wireInstructionsDataTrackingRef :: !TrackingReference,
    wireInstructionsDataBeneficiaryDetails :: !BeneficiaryDetails,
    wireInstructionsDataBeneficiaryBankDetails :: !BeneficiaryBankDetails
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec WireInstructionsData)

instance HasCodec WireInstructionsData where
  codec =
    object "WireInstructionsData" $
      WireInstructionsData
        <$> requiredField' "trackingRef" .= wireInstructionsDataTrackingRef
        <*> requiredField' "beneficiary" .= wireInstructionsDataBeneficiaryDetails
        <*> requiredField' "beneficiaryBank" .= wireInstructionsDataBeneficiaryBankDetails

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
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec WireAccountData)

instance HasCodec WireAccountData where
  codec =
    object "WireAccountData" $
      WireAccountData
        <$> requiredField' "id" .= wireAccountDataId
        <*> requiredField' "status" .= wireAccountDataStatus
        <*> requiredField' "description" .= wireAccountDataDescription
        <*> requiredField' "trackingRef" .= wireAccountDataTrackingRef
        <*> requiredField' "fingerprint" .= wireAccountDataFingerprint
        <*> requiredField' "billingDetails" .= wireAccountDataBillingDetails
        <*> requiredField' "bankAddress" .= wireAccountDataBankAddress
        <*> requiredField' "createDate" .= wireAccountDataCreateDate
        <*> requiredField' "updateDate" .= wireAccountDataUpdateDate

---------------------------------------------------------------
-- Shared types
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

data PaymentStatus = PaymentPending | Confirmed | Paid | PaymentFailed | ActionRequired
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

-- TODO can we do type narrowing to have other types that represent subsets of this one without have to write
-- custom constructors?
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
  -- TODO this should be a numeric type, maybe?
  { unAmount :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec Amount where
  codec = dimapCodec Amount unAmount codec

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
    riskEvaluationReason :: !Text -- TODO probably fine, but maybe just give it a custom type to avoid too much text?
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

newtype AddressLine = AddressLine
  { unAddressLine :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec AddressLine where
  codec = dimapCodec AddressLine unAddressLine codec

-- See 'accountNumberRegex' for where these numeric constraints come from
--
-- We're using greater than, less than without equality assertions in order
-- to better support 'Refined.weaken'
-- type AccountNumberConstraints =
--   SizeGreaterThan 3
--     && SizeLessThan 18
-- TODO may need to derive a HasCodec instance for this to let me use constraints

-- newtype AccountNumber = AccountNumber {unAccountNumber :: Refined AccountNumberConstraints Text}
newtype AccountNumber = AccountNumber {unAccountNumber :: Text}
  deriving stock (Eq, Show, Lift)
  deriving newtype (ToJSON)

-- Account numbers can have capital letters or digits
accountNumberRegex :: Regex
accountNumberRegex = [re|^[A-Z0-9]{4,17}$|]

mkAccountNumber :: Text -> Maybe AccountNumber
mkAccountNumber t =
  if t =~ accountNumberRegex
    then -- then AccountNumber <$> refineFail t
      Just (AccountNumber t)
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

-- TODO get this working with refined types
instance HasCodec AccountNumber where
  codec = dimapCodec AccountNumber unAccountNumber codec

-- where
--   accountNumberCodec =
--     bimapCodec
--       (mapLeft ("Invalid AccountNumber: " ++) . mkAccountNumber)
--       accountNumberToText
--       codec

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

newtype City = City
  { unCity :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec City where
  codec = dimapCodec City unCity codec

-- TODO add constraints maybe?  Risk here is that I block valid codes.
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

-- TODO add constraints (should district be state?  Can I just use an enum?)
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

-- TODO maybe constrain this?  Not sure what it looks like yet so that can happen later
newtype TrackingReference = TrackingReference
  { unTrackingReference :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec TrackingReference where
  codec = dimapCodec TrackingReference unTrackingReference codec

-- TODO maybe add validation?  I don't know enough about it yet.
newtype HexString = HexString
  { unHexString :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec HexString where
  codec = dimapCodec HexString unHexString codec

-- TODO add validation if necessary
newtype CircleId = CircleId
  { unCircleId :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec CircleId where
  codec = dimapCodec CircleId unCircleId codec

-- TODO add validation if necessary
newtype WalletId = WalletId
  { unWalletId :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance HasCodec WalletId where
  codec = dimapCodec WalletId unWalletId codec

-- TODO, consider replacing this hand-rolled validation with the Data.UUID library if you really care about accurate UUIDs.
-- This is fine for now, though.
newtype UUID = UUID
  { unUUID :: Text
  }
  deriving (Eq, Show, Lift, Generic)
  deriving newtype (ToJSON)

-- TODO this doesn't appear to work
instance Validity UUID where
  validate (UUID n) = check (isNothing $ mkUUID n) "The UUID has a valid format."

-- instance IsString UUID where
--   fromString = mkUUID . fromString

instance HasCodec UUID where
  -- TODO how do we apply the smart constructor here?
  -- could options here: we could still derive ToJSON and FromJSON normally and just have a HasCodec instance here
  -- or, we could try and build the smart constructor into the codec
  codec :: JSONCodec UUID
  codec = dimapCodec UUID unUUID codec

instance FromJSON UUID where
  parseJSON = withText "UUID" $ \t ->
    case mkUUID t of
      Nothing -> fail $ "Invalid UUID: " ++ T.unpack t
      Just uuid -> pure uuid

uuidRegex :: Regex
uuidRegex = [re|^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$|]

mkUUID :: Text -> Maybe UUID
mkUUID t =
  if t =~ uuidRegex
    then Just (UUID t)
    else Nothing

compileUUID :: QuasiQuoter
compileUUID =
  QuasiQuoter
    { quoteExp = compileUUID',
      quotePat = error "UUID is not a pattern; use uuidToText instead",
      quoteDec = error "UUID is not supported at top-level",
      quoteType = error "UUID is not supported as a type"
    }
  where
    compileUUID' :: String -> Q Exp
    compileUUID' s = case mkUUID (T.pack s) of
      Nothing -> fail ("Invalid UUID: " ++ s ++ ". Must match the UUID spec, with no other characters.")
      Just uuid -> [|uuid|]

---------------------------------------------------------------
-- Utils
---------------------------------------------------------------

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
-- Payments API
-- this could probably be a new module
---------------------------------------------------------------

---------------------------------------------------------------
-- Payments endpoints
---------------------------------------------------------------

data PaymentRequest

type instance CircleRequest PaymentRequest = CircleResponseBody (ThisOrThat FiatOrCryptoPaymentResponse FiatCancelOrRefund)

data PaymentsRequest

type instance CircleRequest PaymentsRequest = CircleResponseBody [ThisOrThat FiatOrCryptoPaymentResponse FiatCancelOrRefund]

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

data CreatePaymentBody = CreatePaymentBody
  { createPaymentIdempotencyKey :: !UUID,
    createPaymentKeyId :: !Text, -- TODO this is actually a UUID, but in Sandbox it has to be `key1`.  Figure out how to reconcile this later.
    createMetadata :: !CreateMetadata,
    createPaymentAmount :: !MoneyAmount,
    createPaymentAutoCapture :: !(Maybe Bool), -- TODO how to add default?
    createPaymentVerification :: !VerificationType,
    createPaymentVerificationSuccessUrl :: !(Maybe Text), -- TODO depends on if VerificationType = ThreeDSecure
    createPaymentVerificationFailureUrl :: !(Maybe Text), -- TODO depends on if VerificationType = ThreeDSecure
    createPaymentSource :: !PaymentSource,
    createPaymentDescription :: !(Maybe Text),
    createPaymentEncryptedData :: !(Maybe Text), -- TODO check that it contains the CVV somehow
    createPaymentChannel :: !(Maybe Text)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreatePaymentBody)

instance HasCodec CreatePaymentBody where
  codec =
    object "CreatePaymentBody" $
      CreatePaymentBody
        <$> requiredField' "idempotencyKey" .= createPaymentIdempotencyKey
        <*> requiredField' "keyId" .= createPaymentKeyId
        <*> requiredField' "metadata" .= createMetadata
        <*> requiredField' "amount" .= createPaymentAmount
        <*> optionalField' "autoCapture" .= createPaymentAutoCapture
        <*> requiredField' "verification" .= createPaymentVerification
        <*> optionalField' "verificationSuccessfulUrl" .= createPaymentVerificationSuccessUrl
        <*> optionalField' "verificationFailureUrl" .= createPaymentVerificationFailureUrl
        <*> requiredField' "source" .= createPaymentSource
        <*> optionalField' "description" .= createPaymentDescription
        <*> optionalField' "encryptedData" .= createPaymentEncryptedData
        <*> optionalField' "channel" .= createPaymentChannel

data CreateMetadata = CreateMetadata
  { createMetadataEmail :: !Text, -- TODO this screams newtype w/ smart constructor
    createMetadataPhoneNumber :: !(Maybe Text), -- TODO this screams newtype w/ smart constructor
    createMetadataSessionId :: !Text, -- TODO hash newtype or something
    createMetadataIpAddress :: !Text -- TODO should be a newtype
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreateMetadata)

instance HasCodec CreateMetadata where
  codec =
    object "CreateMetadata" $
      CreateMetadata
        <$> requiredField' "email" .= createMetadataEmail
        <*> optionalField' "phoneNumber" .= createMetadataPhoneNumber
        <*> requiredField' "sessionId" .= createMetadataSessionId
        <*> requiredField' "ipAddress" .= createMetadataIpAddress

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

-- TODO could likely make a better abstraction here

-- | A FiatOrCryptoPaymentResponse object represents a fiat or crypto payment.  These payments look identical
-- except for the "Description" field, and the fact that a FiatPayment could have response verification data, whereas
-- a crypto payment could have info about the deposit address, transaction hash etc.
-- I'd love to differentiate these fields based on what I can parse from JSON, but there's enough overlap between
-- the two response bodies that I can cheat for now.
data FiatOrCryptoPaymentResponse = FiatOrCryptoPaymentResponse
  { -- the following fields will be present on every response
    fiatOrCryptoPaymentId :: !UUID,
    fiatOrCryptoPaymentType :: !PaymentType,
    fiatOrCryptoPaymentMerchantId :: !UUID,
    fiatOrCryptoPaymentMerchantWalletId :: !WalletId,
    fiatOrCryptoPaymentAmount :: !MoneyAmount,
    fiatOrCryptoPaymentSource :: !PaymentSource,
    fiatOrCryptoPaymentDescription :: !Text, -- TODO this should be an enum that just says "Payment"
    fiatOrCryptoPaymentStatus :: !PaymentStatus,
    -- the following fields will only be present on Crypto payments
    fiatOrCryptoPaymentPaymentIntentId :: !(Maybe UUID),
    fiatOrCryptoPaymentSettlementAmount :: !(Maybe MoneyAmount), -- TODO this will probably change to not use Centi
    fiatOrCryptoPaymentDepositAddress :: !(Maybe PaymentDepositAddress),
    fiatOrCryptoPaymentTransactionHash :: !(Maybe Text), -- TODO this is probably a HexString too
    -- the following fields will only be present on fiat payments
    fiatOrCryptoPaymentVerification :: !(Maybe VerificationData),
    fiatOrCryptoPaymentCaptured :: !(Maybe Bool),
    fiatOrCryptoPaymentCaptureAmount :: !(Maybe MoneyAmount),
    fiatOrCryptoPaymentCaptureDate :: !(Maybe UTCTime),
    fiatOrCryptoPaymentRequiredAction :: !(Maybe PaymentActionRequired),
    fiatOrCryptoPaymentCancel :: !(Maybe FiatCancelOrRefund),
    fiatOrCryptoPaymentRefunds :: !(Maybe [FiatCancelOrRefund]),
    fiatOrCryptoPaymentFees :: !(Maybe MoneyAmount),
    fiatOrCryptoPaymentChannel :: !(Maybe Text), -- TODO this needs a type
    fiatOrCryptoPaymentCreateDate :: !(Maybe UTCTime),
    fiatOrCryptoPaymentUpdateDate :: !(Maybe UTCTime),
    fiatOrCryptoPaymentTrackingRef :: !(Maybe TrackingReference),
    fiatOrCryptoPaymentErrorCode :: !(Maybe PaymentErrorCode),
    fiatOrCryptoMetadata :: !(Maybe Metadata),
    fiatOrCryptoPaymentRiskEvaluation :: !(Maybe RiskEvaluation)
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec FiatOrCryptoPaymentResponse)

instance HasCodec FiatOrCryptoPaymentResponse where
  codec =
    object "FiatOrCryptoPaymentResponse" $
      FiatOrCryptoPaymentResponse
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

data Metadata = Metadata
  { metadataEmail :: !Text, -- TODO this screams newtype w/ smart constructor
    metadataPhoneNumber :: !(Maybe Text) -- TODO this screams newtype w/ smart constructor
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec Metadata)

instance HasCodec Metadata where
  codec =
    object "Metadata" $
      Metadata
        <$> requiredField' "email" .= metadataEmail
        <*> optionalField' "phoneNumber" .= metadataPhoneNumber

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

data AVS = AVSNotRequested | AVSPending
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec AVS)

instance HasCodec AVS where
  codec = stringConstCodec $ NE.fromList [(AVSNotRequested, "not_requested"), (AVSPending, "pending")]

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
    paymentDepositAddressAddress :: !HexString -- TODO this may be a hex string
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

-- | A FiatCancelOrRefund object represents an attempt at canceling or refunding a payment.
-- Cancellations apply only to card payments, and its presence doesn't necessarily mean that the cancellation was successful.
-- A successful cancellation has a status of paid, a successful refund has a status of confirmed.
-- TODO I could likely do some better data modeling here, there's a ton of shared fields between these
-- types so I kinda cheated and just made one mega type with maybes, but it'll be more ergonomic for devs
-- to have a specific type that's generated from the parsing.  The tricky part is the differentiator is the
-- field `type`, so I think I'll need to be clever about this.
data FiatCancelOrRefund = FiatCancelOrRefund
  { fiatCancelOrRefundId :: !UUID,
    fiatCancelOrRefundType :: !PaymentType,
    fiatCancelOrRefundMerchantId :: !UUID,
    fiatCancelOrRefundMerchantWalletId :: !WalletId,
    fiatCancelOrRefundAmount :: !MoneyAmount,
    fiatCancelOrRefundSource :: !PaymentSource,
    fiatCancelOrRefundDescription :: !Text, -- TODO description enum
    fiatCancelOrRefundStatus :: !PaymentStatus,
    fiatCancelOrRefundOriginalPayment :: !OriginalFiatPayment,
    fiatCancelOrRefundFees :: !(Maybe MoneyAmount),
    fiatCancelOrRefundChannel :: !(Maybe Text),
    fiatCancelOrRefundReason :: !(Maybe CancelPaymentReason), -- TODO feels like something that could have an enum
    fiatCancelOrRefundCreateDate :: !UTCTime,
    fiatCancelOrRefundUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec FiatCancelOrRefund)

instance HasCodec FiatCancelOrRefund where
  codec =
    object "FiatCancelOrRefund" $
      FiatCancelOrRefund
        <$> requiredField' "id" .= fiatCancelOrRefundId
        <*> requiredField' "type" .= fiatCancelOrRefundType
        <*> requiredField' "merchantId" .= fiatCancelOrRefundMerchantId
        <*> requiredField' "merchantWalletId" .= fiatCancelOrRefundMerchantWalletId
        <*> requiredField' "amount" .= fiatCancelOrRefundAmount
        <*> requiredField' "source" .= fiatCancelOrRefundSource
        <*> requiredField' "description" .= fiatCancelOrRefundDescription
        <*> requiredField' "status" .= fiatCancelOrRefundStatus
        <*> requiredField' "originalPayment" .= fiatCancelOrRefundOriginalPayment
        <*> optionalField' "fees" .= fiatCancelOrRefundFees
        <*> optionalField' "channel" .= fiatCancelOrRefundChannel
        <*> optionalField' "reason" .= fiatCancelOrRefundReason
        <*> requiredField' "createDate" .= fiatCancelOrRefundCreateDate
        <*> requiredField' "updateDate" .= fiatCancelOrRefundUpdateDate

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
    paymentActionRequiredRedirectUrl :: !Text -- TODO URL type
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

data CancelPaymentBody = CancelPaymentBody
  { cancelPaymentIdempotencyKey :: !UUID,
    cancelPaymentReason :: !(Maybe CancelPaymentReason)
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec CancelPaymentBody)

instance HasCodec CancelPaymentBody where
  codec =
    object "CancelPaymentBody" $
      CancelPaymentBody
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

data RefundPaymentBody = RefundPaymentBody
  { refundPaymentIdempotencyKey :: !UUID,
    refundPaymentAmount :: MoneyAmount,
    refundPaymentReason :: !(Maybe CancelPaymentReason)
  }
  deriving (Eq, Show)
  deriving
    ( ToJSON,
      FromJSON
    )
    via (Autodocodec RefundPaymentBody)

instance HasCodec RefundPaymentBody where
  codec =
    object "RefundPaymentBody" $
      RefundPaymentBody
        <$> requiredField' "idempotencyKey" .= refundPaymentIdempotencyKey
        <*> requiredField' "amount" .= refundPaymentAmount
        <*> optionalField' "reason" .= refundPaymentReason

---------------------------------------------------------------
-- On-chain payments endpoints
---------------------------------------------------------------

data OnChainTransferRequest

type instance CircleRequest OnChainTransferRequest = CircleResponseBody TransferData

instance CircleHasParam OnChainTransferRequest ReturnIdentitiesQueryParam

data OnChainTransfersRequest

type instance CircleRequest OnChainTransfersRequest = CircleResponseBody [TransferData]

instance CircleHasParam OnChainTransfersRequest PaginationQueryParams

instance CircleHasParam OnChainTransfersRequest FromQueryParam

instance CircleHasParam OnChainTransfersRequest ToQueryParam

instance CircleHasParam OnChainTransfersRequest PageSizeQueryParam

instance CircleHasParam OnChainTransfersRequest WalletIdQueryParam

instance CircleHasParam OnChainTransfersRequest SourceWalletIdQueryParam

instance CircleHasParam OnChainTransfersRequest DestinationWalletIdQueryParam

instance CircleHasParam OnChainTransfersRequest ReturnIdentitiesQueryParam

data OnChainAddressRequest

type instance CircleRequest OnChainAddressRequest = CircleResponseBody DepositAddressData

data OnChainTransferBodyParams = OnChainTransferBodyParams
  { onChainTransferBodyParamsIdempotencyKey :: !UUID,
    onChainTransferBodyParamsSource :: !SourceWallet,
    onChainTransferBodyParamsDestination :: !(ThisOrThat DestinationWallet DestinationBlockchain),
    onChainTransferBodyParamsAmount :: !MoneyAmount
  }
  deriving (Eq, Show)

instance ToJSON OnChainTransferBodyParams where
  toJSON :: OnChainTransferBodyParams -> Aeson.Value
  toJSON OnChainTransferBodyParams {..} =
    Aeson.object
      [ "idempotencyKey" Aeson..= onChainTransferBodyParamsIdempotencyKey,
        "source" Aeson..= onChainTransferBodyParamsSource,
        "destination" Aeson..= onChainTransferBodyParamsDestination,
        "amount" Aeson..= onChainTransferBodyParamsAmount
      ]

---------------------------------------------------------------
-- Card endpoints
---------------------------------------------------------------

data CardsRequest

type instance CircleRequest CardsRequest = CircleResponseBody [CardData]

instance CircleHasParam CardsRequest PaginationQueryParams

instance CircleHasParam CardsRequest PageSizeQueryParam

data CardRequest

type instance CircleRequest CardRequest = CircleResponseBody CardData

data ListCardData = ListCardData
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
    listCardRiskEvaluation :: !RiskEvaluation,
    listCardCreateDate :: !UTCTime,
    listCardUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ListCardData)

instance HasCodec ListCardData where
  codec =
    object "ListCardData" $
      ListCardData
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
        <*> requiredField' "riskEvaluation" .= listCardRiskEvaluation
        <*> requiredField' "createDate" .= listCardCreateDate
        <*> requiredField' "updateDate" .= listCardUpdateDate

data CardData = CardData
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
    cardRiskEvaluation :: !RiskEvaluation,
    cardMetadata :: !Metadata,
    cardCreateDate :: !UTCTime,
    cardUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CardData)

instance HasCodec CardData where
  codec =
    object "CardData" $
      CardData
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
        <*> requiredField' "riskEvaluation" .= cardRiskEvaluation
        <*> requiredField' "metadata" .= cardMetadata
        <*> requiredField' "createDate" .= cardCreateDate
        <*> requiredField' "updateDate" .= cardUpdateDate

data CreateCardBodyParams = CreateCardBodyParams
  { createCardIdempotencyKey :: !UUID,
    createCardKeyId :: !(Maybe Text), -- key1 in sandbox
    createCardEncryptedData :: !(Maybe Text), -- TODO check that it contains the CVV AND the card number somehow
    createCardBillingDetails :: !BillingDetails,
    createCardExpiryMonth :: !Int,
    createCardExpiryYear :: !Int,
    createCardMetadata :: !CreateMetadata
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreateCardBodyParams)

instance HasCodec CreateCardBodyParams where
  codec =
    object "CreateCardBodyParams" $
      CreateCardBodyParams
        <$> requiredField' "idempotencyKey" .= createCardIdempotencyKey
        <*> optionalField' "keyId" .= createCardKeyId
        <*> requiredField' "encryptedData" .= createCardEncryptedData
        <*> requiredField' "billingDetails" .= createCardBillingDetails
        <*> requiredField' "expMonth" .= createCardExpiryMonth
        <*> requiredField' "expYear" .= createCardExpiryYear
        <*> requiredField' "metadata" .= createCardMetadata

data UpdateCardBodyParams = UpdateCardBodyParams
  { updateCardKeyId :: !(Maybe Text), -- key1 in sandbox
    updateCardEncryptedData :: !(Maybe Text), -- TODO check that it contains the CVV AND the card number somehow
    updateCardExpiryMonth :: !Int,
    updateCardExpiryYear :: !Int
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec UpdateCardBodyParams)

instance HasCodec UpdateCardBodyParams where
  codec =
    object "UpdateCardBodyParams" $
      UpdateCardBodyParams
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
-- ACH endpoints
---------------------------------------------------------------

data ACHBankAccountRequest

type instance CircleRequest ACHBankAccountRequest = CircleResponseBody ACHBankAccountData

data ACHBankAccountData = ACHBankAccountData
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
    achBankAccountMetadata :: !Metadata,
    achBankAccountCreateDate :: !UTCTime,
    achBankAccountUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec ACHBankAccountData)

instance HasCodec ACHBankAccountData where
  codec =
    object "ACHBankAccountData" $
      ACHBankAccountData
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

data CreateACHBankAccountBodyParams = CreateACHBankAccountBodyParams
  { achBankAccountBodyIdempotencyKey :: !UUID,
    achBankAccountBodyPlaidProcessorToken :: !Text, -- TODO newtype
    achBankAccountBodyBillingDetails :: !BillingDetails,
    achBankAccountBodyBankAccountType :: !(Maybe ACHBankAccountType),
    achBankAccountBodyMetadata :: !CreateMetadata
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreateACHBankAccountBodyParams)

instance HasCodec CreateACHBankAccountBodyParams where
  codec =
    object "CreateACHBankAccountBodyParams" $
      CreateACHBankAccountBodyParams
        <$> requiredField' "idempotencyKey" .= achBankAccountBodyIdempotencyKey
        <*> requiredField' "plaidProcessorToken" .= achBankAccountBodyPlaidProcessorToken
        <*> requiredField' "billingDetails" .= achBankAccountBodyBillingDetails
        <*> optionalField' "bankAccountType" .= achBankAccountBodyBankAccountType
        <*> requiredField' "metadata" .= achBankAccountBodyMetadata

data CreateMockACHBankAccountBodyParams = CreateMockACHBankAccountBodyParams
  { mockACHBankAccountBodyAccount :: !MockACHBankAccount,
    mockACHBankAccountBodyBalance :: MoneyAmount
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec CreateMockACHBankAccountBodyParams)

instance HasCodec CreateMockACHBankAccountBodyParams where
  codec =
    object "CreateMockACHBankAccountBodyParams" $
      CreateMockACHBankAccountBodyParams
        <$> requiredField' "account" .= mockACHBankAccountBodyAccount
        <*> requiredField' "balance" .= mockACHBankAccountBodyBalance

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
-- SEPA endpoint
---------------------------------------------------------------

data SEPAAccountRequest

type instance CircleRequest SEPAAccountRequest = CircleResponseBody SEPAAccountData

data SEPAInstructionsRequest

type instance CircleRequest SEPAInstructionsRequest = CircleResponseBody WireInstructionsData -- reuse this type
data SEPAAccountBodyParams = SEPAAccountBodyParams
  { sepaAccountBodyParamsIdempotencyKey :: !UUID,
    sepaAccountBodyParamsIBAN :: !Text, -- TODO newtype IBAN
    sepaAccountBodyParamsBillingDetails :: !BillingDetails
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SEPAAccountBodyParams)

instance HasCodec SEPAAccountBodyParams where
  codec =
    object "SEPAAccountBodyParams" $
      SEPAAccountBodyParams
        <$> requiredField' "idempotencyKey" .= sepaAccountBodyParamsIdempotencyKey
        <*> requiredField' "iban" .= sepaAccountBodyParamsIBAN
        <*> requiredField' "billingDetails" .= sepaAccountBodyParamsBillingDetails

data SEPAAccountData = SEPAAccountData
  { sepaAccountDataId :: !UUID,
    sepaAccountDataStatus :: !Status,
    sepaAccountDataDescription :: !Text, -- TODO better type: Bank name plus last four digits of the bank account number or IBAN.  Make a custom type for this
    sepaAccountDataTrackingRef :: !TrackingReference,
    sepaAccountDataFingerprint :: !UUID, -- TODO newtype this
    sepaAccountDataRiskEvaluation :: !(Maybe RiskEvaluation),
    sepaAccountDataBillingDetails :: !BillingDetails,
    sepaAccountDataCreateDate :: !UTCTime,
    sepaAccountDataUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SEPAAccountData)

instance HasCodec SEPAAccountData where
  codec =
    object "SEPAAccountData" $
      SEPAAccountData
        <$> requiredField' "id" .= sepaAccountDataId
        <*> requiredField' "status" .= sepaAccountDataStatus
        <*> requiredField' "description" .= sepaAccountDataDescription
        <*> requiredField' "trackingRef" .= sepaAccountDataTrackingRef
        <*> requiredField' "fingerprint" .= sepaAccountDataFingerprint
        <*> optionalField' "riskEvaluation" .= sepaAccountDataRiskEvaluation
        <*> requiredField' "billingDetails" .= sepaAccountDataBillingDetails
        <*> requiredField' "createDate" .= sepaAccountDataCreateDate
        <*> requiredField' "updateDate" .= sepaAccountDataUpdateDate

---------------------------------------------------------------
-- Settlements Endpoint
---------------------------------------------------------------

data SettlementRequest

type instance CircleRequest SettlementRequest = CircleResponseBody SettlementData

data SettlementsRequest

type instance CircleRequest SettlementsRequest = CircleResponseBody [SettlementData]

instance CircleHasParam SettlementsRequest PaginationQueryParams

instance CircleHasParam SettlementsRequest FromQueryParam

instance CircleHasParam SettlementsRequest ToQueryParam

instance CircleHasParam SettlementsRequest PageSizeQueryParam

data SettlementData = SettlementData
  { settlementDataId :: !UUID,
    settlementDataMerchantWalletId :: !UUID,
    settlementDataWalletId :: !UUID,
    settlementDataTotalDebits :: !MoneyAmount,
    settlementDataTotalCredits :: !MoneyAmount,
    settlementDataPaymentFees :: !MoneyAmount,
    settlementDataChargebackFees :: !MoneyAmount,
    settlementDataCreateDate :: !UTCTime,
    settlementDataUpdateDate :: !UTCTime
  }
  deriving (Eq, Show)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec SettlementData)

instance HasCodec SettlementData where
  codec =
    object "SettlementData" $
      SettlementData
        <$> requiredField' "id" .= settlementDataId
        <*> requiredField' "merchantWalletId" .= settlementDataMerchantWalletId
        <*> requiredField' "walletId" .= settlementDataWalletId
        <*> requiredField' "totalDebits" .= settlementDataTotalDebits
        <*> requiredField' "totalCredits" .= settlementDataTotalCredits
        <*> requiredField' "paymentFees" .= settlementDataPaymentFees
        <*> requiredField' "chargebackFees" .= settlementDataChargebackFees
        <*> requiredField' "createDate" .= settlementDataCreateDate
        <*> requiredField' "updateDate" .= settlementDataUpdateDate