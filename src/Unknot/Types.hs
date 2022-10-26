{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    -- Wires Endpoint
    WireAccountDetails (..),
    WireAccountData (..),
    WireInstructionsData (..),
    WireAccountRequest,
    WireAccountsRequest,
    WireInstructionsRequest,
    -- Balance Endpoint
    BalanceRequest,
    -- Payouts Endpoint
    PayoutsRequest,
    PayoutRequest,
    PayoutData (..),
    PayoutDetails (..),
    -- Shared types across different endpoints
    DestinationBankAccount (..),
    USDOrEURAmount (..),
    PayoutQueryParameters (..),
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
    ErrorCode (..),
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
    ToJSON (toJSON),
    Value (Array, Null, String),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
  )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Coerce (coerce)
import Data.Fixed (Centi, Fixed (MkFixed))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
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

data CircleResponse a = CircleResponse
  { circleResponseCode :: Maybe ResponseStatus,
    circleResponseMessage :: Maybe ResponseMessage,
    circleResponseData :: Maybe a
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
-- Balance endpoints
---------------------------------------------------------------

data BalanceRequest

type instance CircleRequest BalanceRequest = CircleResponse BalanceData

data BalanceData = BalanceData
  { balanceDataAvailable :: ![CurrencyBalance],
    balanceDataUnsettled :: ![CurrencyBalance]
  }
  deriving (Show)

instance FromJSON BalanceData where
  parseJSON = withObject "BalanceData" parse
    where
      parse o =
        BalanceData
          <$> o .: "available"
          <*> o .: "unsettled"

data CurrencyBalance = CurrencyBalance
  { currencyBalanceAmount :: !Amount,
    currencyBalanceCurrency :: !AllowedCurrencies
  }
  deriving (Show)

instance FromJSON CurrencyBalance where
  parseJSON = withObject "CurrencyBalance" parse
    where
      parse o =
        CurrencyBalance
          <$> o .: "amount"
          <*> o .: "currency"

---------------------------------------------------------------
-- Payout endpoints
---------------------------------------------------------------
data PayoutRequest

type instance CircleRequest PayoutRequest = CircleResponse PayoutData

data PayoutsRequest

type instance CircleRequest PayoutsRequest = CircleResponse [PayoutData]

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
    payoutDataErrorCode :: !ErrorCode,
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
          <*> o .: "payoutErrorCode"
          <*> o .: "riskEvaluation"
          <*> o .: "adjustments"
          <*> o .: "payoutReturn"
          <*> o .: "createDate"
          <*> o .: "updateDate"

-- Circle has some BS pagination where they let users supply some canonical
-- collection ID, and then this pagination rule will return `n` entries before OR after that page,
-- where `n` is controlled by the pageSize param.  This type exists to prevent callers from providing both params, which would error out
data Pagination = PageBefore !UUID | PageAfter !UUID deriving (Show, Eq)

instance ToJSON Pagination where
  toJSON (PageAfter a) = String (unUUID a)
  toJSON (PageBefore a) = String (unUUID a)

data PayoutQueryParameters = PayoutQueryParameters
  { payoutQueryParametersDestination :: !(Maybe UUID),
    -- intentionally omitting the following parameter Circle's API only supports one type and includes it by default
    payoutQueryParameterType :: !Text, -- TODO this type is the string literal "Wire".
    payoutQueryParametersPayoutStatus :: !(Maybe [Status]),
    payoutQueryParametersFrom :: !(Maybe UTCTime), -- TODO test that UTCTime works, needs actual payouts first bc currently returns empty data
    payoutQueryParametersTo :: !(Maybe UTCTime),
    payoutQueryParameters :: !(Maybe Pagination),
    payoutQueryParametersPageSize :: !(Maybe Int)
  }

instance ToJSON PayoutQueryParameters where
  toJSON PayoutQueryParameters {..} =
    omitNulls
      [ "destination" .= payoutQueryParametersDestination,
        "status" .= payoutQueryParametersPayoutStatus,
        "from" .= payoutQueryParametersFrom,
        "to" .= payoutQueryParametersTo,
        "pageSize" .= payoutQueryParametersPageSize
      ]

data PayoutDetails = PayoutDetails
  { payoutDetailsIdempotencyKey :: !UUID,
    payoutDetailsDestination :: !DestinationBankAccount,
    payoutDetailsAmount :: !USDOrEURAmount
  }
  deriving (Eq, Show)

instance ToJSON PayoutDetails where
  toJSON PayoutDetails {..} =
    object
      [ "idempotencyKey" .= payoutDetailsIdempotencyKey,
        "destination" .= payoutDetailsDestination,
        "amount" .= payoutDetailsAmount
      ]

---------------------------------------------------------------
-- Wire endpoints
---------------------------------------------------------------

data WireAccountRequest

type instance CircleRequest WireAccountRequest = CircleResponse WireAccountData

data WireAccountsRequest

type instance CircleRequest WireAccountsRequest = CircleResponse [WireAccountData]

data WireInstructionsRequest

type instance CircleRequest WireInstructionsRequest = CircleResponse WireInstructionsData

data WireAccountDetails = WireAccountDetails
  { idempotencyKey :: !UUID,
    accountNumber :: !AccountNumber,
    routingNumber :: !RoutingNumber,
    billingDetails :: !BillingDetails,
    bankAddress :: !BankAddress
  }
  deriving (Eq, Show)

instance ToJSON WireAccountDetails where
  toJSON WireAccountDetails {..} =
    object
      [ "idempotencyKey" .= idempotencyKey,
        "accountNumber" .= accountNumber,
        "routingNumber" .= routingNumber,
        "billingDetails" .= billingDetails,
        "bankAddress" .= bankAddress
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

data ErrorCode
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

instance ToJSON ErrorCode where
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

instance FromJSON ErrorCode where
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
data AllowedCurrencies = USD | EUR | BTC | ETH deriving (Eq, Show)

instance ToJSON AllowedCurrencies where
  toJSON USD = String "USD"
  toJSON EUR = String "EUR"
  toJSON BTC = String "BTC"
  toJSON ETH = String "ETH"

instance FromJSON AllowedCurrencies where
  parseJSON (String s) = case T.unpack s of
    "USD" -> return USD
    "EUR" -> return EUR
    "BTC" -> return BTC
    "ETH" -> return ETH
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
  deriving (Show)

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

data BankAddress = BankAddress
  { bankAddressBankName :: !(Maybe Text),
    bankAddressBankCity :: !(Maybe City),
    bankAddressBankCountry :: !(Maybe ISO3166Alpha2),
    bankAddressBankLine1 :: !(Maybe AddressLine),
    bankAddressBankLine2 :: !(Maybe AddressLine),
    bankAddressBankDistrict :: !(Maybe District)
  }
  deriving (Eq, Show)

instance ToJSON BankAddress where
  toJSON BankAddress {..} =
    omitNulls
      [ "name" .= bankAddressBankName,
        "city" .= bankAddressBankCity,
        "country" .= bankAddressBankCountry,
        "line1" .= bankAddressBankLine1,
        "line2" .= bankAddressBankLine2,
        "district" .= bankAddressBankDistrict
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