{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StrictData                 #-}

module Unknot.Types
  ( ApiToken (..)
  , SKU (..)
  , mkSku
  , Quantity (..)
  , AddressLine (..)
  , AccountNumber (..)
  , RoutingNumber (..)
  , City (..)
  , PostalCode (..)
  , District (..)
  , BankAddress (..)
  , BillingDetails (..)
  , BeneficiaryBankDetails (..)
  , BeneficiaryDetails
  , SwiftCode (..)
  , Region (..)
  , Country (..)
  , Currency (..)
  , ResponseStatus (..)
  , ResponseMessage (..)
  , Error (..)
  , ErrorCode (..)
  , ErrorMessage (..)
  , ErrorType (..)
  , Id (..)
  , ExternalId (..)
  , Reply
  , Method
  , CircleAPIRequest (..)
  , CircleResponse (..)
  , mkCircleAPIRequest
  , WireAccountDetails (..)
  , WireAccountData (..)
  , WireInstructionsData (..)
  , WireAccountRequest
  , WireAccountsRequest
  , WireInstructionsRequest
  , BalanceRequest
  , CircleRequest
  , Host
  , CircleHost(..)
  , CircleConfig (..)
  , hostUri
  , prodEnvConfig
  , sandboxEnvConfig
  , Params (..)
  , Body (..)
  , Query (..)
  , TupleBS8
  , (-&-)
  ) where

import           Control.Applicative ((<|>))
import Data.Aeson
    ( (.:),
      (.:?),
      withObject,
      withText,
      object,
      FromJSON(parseJSON),
      Value(String, Array, Null),
      KeyValue((.=)),
      ToJSON(toJSON) )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock ( UTCTime )
import qualified Data.Vector as V
import Network.HTTP.Client ( Response )
import qualified Network.HTTP.Types.Method as NHTM
import           System.Environment (getEnv)

-- | Token type used for HTTP Bearer authentication.
newtype ApiToken = ApiToken
  { unApiToken :: BS8.ByteString
  } deriving (Read, Show, Eq)


data CircleAPIRequest a b c = CircleAPIRequest
  { rMethod  :: Method -- ^ Method of CircleAPIRequest
  , endpoint :: Text -- ^ Endpoint of CircleAPIRequest
  , params   :: Params TupleBS8 BSL.ByteString -- ^ Request params of CircleAPIRequest
  } deriving (Show)

mkCircleAPIRequest :: Method
                  -> Text
                  -> Params TupleBS8 BSL.ByteString
                  -> CircleAPIRequest a b c
mkCircleAPIRequest = CircleAPIRequest

type family CircleRequest a :: *

---------------------------------------------------------------
-- Balance endpoints
-- https://developers.circle.com/reference/listbusinessbalances
---------------------------------------------------------------

data BalanceRequest
type instance CircleRequest BalanceRequest =  CircleResponse BalanceData

data BalanceData =  BalanceData
  { available :: [CurrencyBalances],
    unsettled :: [CurrencyBalances]
  } deriving (Show)

instance FromJSON BalanceData where
  parseJSON = withObject "BalanceData" parse
    where
      parse o = BalanceData
        <$> o .: "available"
        <*> o .: "unsettled"

data CurrencyBalances = CurrencyBalances
  { magnitude :: Text, -- TODO this should be a numeric type
    currency :: Text -- TODO this should be a sum type of allowed currencies: USD, EUR, BTC, ETH
  } deriving (Show)

instance FromJSON CurrencyBalances where
  parseJSON = withObject "CurrencyBalances" parse
    where
      parse o = CurrencyBalances
        <$> o .: "amount"
        <*> o .: "currency" 

---------------------------------------------------------------
-- Wire endpoints 
-- https://developers.circle.com/reference/createbusinesswireaccount
---------------------------------------------------------------

data WireAccountRequest
type instance CircleRequest WireAccountRequest = CircleResponse WireAccountData

data WireAccountsRequest
type instance CircleRequest WireAccountsRequest = CircleResponse [WireAccountData]

data WireInstructionsRequest
type instance CircleRequest WireInstructionsRequest = CircleResponse WireInstructionsData

data WireAccountDetails = WireAccountDetails
  { idempotencyKey :: ExternalId -- UUID type
  , accountNumber  :: AccountNumber -- Account number
  , routingNumber :: RoutingNumber -- routing number
  , billingDetails :: BillingDetails
  , bankAddress :: BankAddress
  } deriving (Eq, Show)

instance ToJSON WireAccountDetails where
  toJSON WireAccountDetails {..} = object ["idempotencyKey" .= idempotencyKey
                                          , "accountNumber" .= accountNumber
                                          , "routingNumber" .= routingNumber
                                          , "billingDetails" .= billingDetails
                                          , "bankAddress" .= bankAddress]

data BillingDetails = BillingDetails
  { billingName :: Text,
    billingCity :: City,
    billingCountry :: Country, -- TODO country code somehow
    billingLine1 :: AddressLine, -- address type
    billingLine2 :: Maybe AddressLine, -- secondary address type
    billingDistrict :: Maybe District, -- could be a state type
    billingPostalCode :: PostalCode -- postal code type
  }
  deriving (Eq, Show)

instance ToJSON BillingDetails where
  toJSON BillingDetails {..} =
    omitNulls
      [ "name" .= billingName,
        "city" .= billingCity,
        "country" .= billingCountry,
        "line1" .= billingLine1,
        "line2" .= billingLine2,
        "district" .= billingDistrict,
        "postalCode" .= billingPostalCode
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
  { bankName :: Maybe Text,
    bankCity :: Maybe City,
    bankCountry :: Maybe Country, -- TODO country code somehow
    bankLine1 :: Maybe AddressLine, -- address type
    bankLine2 :: Maybe AddressLine, -- secondary address type
    bankDistrict :: Maybe District -- could be a state type
  }
  deriving (Eq, Show)

instance ToJSON BankAddress where
  toJSON BankAddress {..} =
    omitNulls
      [ "name" .= bankName,
        "city" .= bankCity,
        "country" .= bankCountry,
        "line1" .= bankLine1,
        "line2" .= bankLine2,
        "district" .= bankDistrict
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
  { beneficiaryDetailsName :: Text,
    beneficiaryDetailsAddress1 :: Maybe AddressLine, -- address type
    beneficiaryDetailsAddress2 :: Maybe AddressLine -- secondary address type
  } deriving (Eq, Show)

instance FromJSON BeneficiaryDetails where
  parseJSON = withObject "BeneficiaryDetails" parse
    where
      parse o =
        BeneficiaryDetails
          <$> o .: "name"
          <*> o .: "address1" -- todo check these
          <*> o .: "address2" -- todo check these


data BeneficiaryBankDetails = BeneficiaryBankDetails
  { beneficiaryBankDetailsName :: Text,
    beneficiaryBankDetailsSwiftCode :: SwiftCode, -- todo this screams custom type
    beneficiaryBankDetailsRoutingNumber :: RoutingNumber,
    beneficiaryBankDetailsAccountNumber :: AccountNumber,
    beneficiaryBankDetailsCurrency :: Currency,
    beneficiaryBankDetailsAddress :: AddressLine,
    beneficiaryBankDetailsCity :: City,
    beneficiaryBankDetailsPostalCode :: PostalCode,
    beneficiaryBankDetailsCountry :: Country
  }
  deriving (Eq, Show)
instance FromJSON BeneficiaryBankDetails where
  parseJSON = withObject "BeneficiaryBankDetails" parse
    where
      parse o =
        BeneficiaryBankDetails
          <$> o .: "name"
          <*> o .: "swiftCode" -- todo check these
          <*> o .: "routingNumber" -- todo check these
          <*> o .: "accountNumber" -- todo check these
          <*> o .: "currency"
          <*> o .: "address" -- todo check these
          <*> o .: "city" -- todo check these
          <*> o .: "postalCode" -- todo check these
          <*> o .: "country" -- todo check these

data WireInstructionsData = WireInstructionsData
  { wireInstructionsDataTrackingRef :: Text, -- this might be typical or at least have a length req
    wireInstructionsDataBeneficiaryDetails :: BeneficiaryDetails,
    wireInstructionsDataBeneficiaryBankDetails :: BeneficiaryBankDetails
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
  { wireAccountDataId :: Text, -- TODO newtype this
    wireAccountDataStatus :: Text, -- TODO enum this
    wireAccountDataDescription :: Text, -- TODO better type
    wireAccountDataTrackingRef :: Text, -- this might be typical or at least have a length req
    wireAccountDataFingerprint :: Text, -- same newtype as id, should be a UUI
    wireAccountDataBillingDetails :: BillingDetails,
    wireAccountDataBankAddress :: BankAddress,
    wireAccountDataCreateDate :: UTCTime,
    wireAccountDataUpdateDate :: UTCTime
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

newtype SKU = SKU
  { unSku :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

mkSku :: Text -> Maybe SKU
mkSku sku
  | T.length sku > 16 = Nothing
  | T.length sku < 1 = Nothing
  | otherwise = Just (SKU sku)

newtype Quantity = Quantity
  { unQuantity :: Integer
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype AddressLine = AddressLine
  { unAddressLine :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype AccountNumber = AccountNumber
  { unAccountNumber :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype RoutingNumber = RoutingNumber
  { unRoutingNumber :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype City = City
  { unCity :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype PostalCode = PostalCode
  { unPostalCode :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype SwiftCode = SwiftCode
  { unSwiftCode :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype Region = Region
  { unRegion :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype District = District
  { unDistrict :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype Country = Country
  { unCountry :: Text
  } deriving (Eq, Show, ToJSON, FromJSON)

data Currency =
  USD
  deriving (Eq, Show)

instance ToJSON Currency where
  toJSON USD = String "USD"

instance FromJSON Currency where
  parseJSON = withText "Currency" parse
    where
      parse "USD" = pure USD
      parse o     = fail $ "Unexpected Currency: " <> show o

omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull (_, Array a) = (not . V.null) a
    notNull _ = True

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

newtype ResponseStatus = ResponseStatus
  { unResponseStatus :: Integer
  }
  deriving (Eq, Show, FromJSON)

newtype ResponseMessage = ResponseMessage
  { unResponseMessage :: Text
  }
  deriving (Eq, Show, FromJSON)

data Error = Error
  { errorCode :: ErrorCode,
    errorMessage :: ErrorMessage,
    errorType :: ErrorType
  }
  deriving (Eq, Show)

instance FromJSON Error where
  parseJSON = withObject "Error" parse
    where
      parse o =
        Unknot.Types.Error
          <$> o .: "code"
          <*> o .: "message"
          <*> o .: "type"

data ErrorCode
  = ErrorCodeText Text
  | ErrorCodeInteger Integer
  deriving (Eq, Show)

instance FromJSON ErrorCode where
  parseJSON o = (ErrorCodeText <$> parseJSON o) <|> (ErrorCodeInteger <$> parseJSON o)

newtype ErrorMessage = ErrorMessage
  { unErrorMessage :: Text
  }
  deriving (Eq, Show, FromJSON)

data ErrorType
  = ErrorWarning
  | ErrorError
  deriving (Eq, Show)

instance FromJSON ErrorType where
  parseJSON = withText "ErrorType" parse
    where
      parse "warning" = pure ErrorWarning
      parse "error" = pure ErrorError
      parse o = fail $ "Unexpected ErrorType: " <> show o

newtype Id = Id
  { unId :: Integer
  }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype ExternalId = ExternalId
  { unExternalId :: Text
  }
  deriving (Eq, Show, ToJSON, FromJSON)

type Reply = Network.HTTP.Client.Response BSL.ByteString
type Method = NHTM.Method

-- | Either production or sandbox API host
type Host = Text

data CircleHost =
    CircleProduction
  | CircleSandbox
  deriving (Eq, Show)

hostUri :: CircleHost -> Text
hostUri CircleProduction = "https://api.circle.com/v1/"
hostUri CircleSandbox =  "https://api-sandbox.circle.com/v1/"

data CircleConfig = CircleConfig
  { host  :: CircleHost
  , token :: ApiToken
  } deriving (Eq, Show)

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

newtype Query = Query {
  unQuery :: TupleBS8
  } deriving (Eq, Show)

newtype Body = Body {
  unBody :: BSL.ByteString
  } deriving (Eq, Show)

-- | Parameters for each request which include both the query and the body of a
-- request
data Params b c = Params
  { paramsBody :: Maybe Body
  , paramsQuery :: [Query]
  } deriving Show

joinQueryParams :: Params b c -> Params b c -> Params b c
joinQueryParams (Params _ xs) (Params b ys) = Params b (xs ++ ys)

-- | Type alias for query parameters
type TupleBS8 = (BS8.ByteString, BS8.ByteString)

-- | Convert a parameter to a key/value
class ToCircleParam param where
  toCircleParam :: param -> Params TupleBS8 c -> Params TupleBS8 c

instance ToCircleParam SKU where
  toCircleParam (SKU i) =
    joinQueryParams $ Params Nothing [Query ("sku", TE.encodeUtf8 i)]

class (ToCircleParam param) => CircleHasParam request param where

-- | Add an optional query parameter
(-&-)
  :: CircleHasParam r param
  => CircleAPIRequest r b c -> param -> CircleAPIRequest r b c
circleAPIRequest -&- param =
  circleAPIRequest
  { params = toCircleParam param (params circleAPIRequest)
  }