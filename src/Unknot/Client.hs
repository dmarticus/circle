{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Unknot.Client where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import Data.UUID
import qualified Data.UUID as UUID
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
import Network.HTTP.Types.Header (hAccept, hContentType)
import qualified Network.HTTP.Types.Method as NHTM
import Unknot.Types

---------------------------------------------------------------
-- /businessAccount/banks/wires endpoint
---------------------------------------------------------------

-- | Create a business bank account for a wire
-- https://developers.circle.com/reference/createbusinesswireaccount
createBusinessWireAccount :: WireAccountRequestBody -> CircleAPIRequest WireAccountRequest TupleBS8 BSL.ByteString
createBusinessWireAccount wireAccountBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/banks/wires"
    params = case wireAccountBody of
      USBankAccount usBankAccountBody -> Params (Just $ Body (encode usBankAccountBody)) []
      IBANBankAccount ibanBankAccountBody -> Params (Just $ Body (encode ibanBankAccountBody)) []
      NonIBANBankAccount nonIBANBankAccountBody -> Params (Just $ Body (encode nonIBANBankAccountBody)) []

-- | Get a list of business account wire accounts
-- https://developers.circle.com/reference/listbusinesswireaccounts
listBusinessWireAccounts :: CircleAPIRequest WireAccountsRequest TupleBS8 BSL.ByteString
listBusinessWireAccounts = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/wires"
    params = Params Nothing []

-- | Get a single business account wire account, accepts the wire account Id as a parameter
-- https://developers.circle.com/reference/getbusinesswireaccount
getBusinessWireAccount :: UUID -> CircleAPIRequest WireAccountRequest TupleBS8 BSL.ByteString
getBusinessWireAccount wireAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/wires/" <> UUID.toText wireAccountId
    params = Params Nothing []

-- | Get the wire transfer instructions into the Circle business bank account given your bank account id.
-- https://developers.circle.com/reference/getbusinesswireaccountinstructions
getBusinessWireAccountInstructions :: UUID -> CircleAPIRequest WireInstructionsRequest TupleBS8 BSL.ByteString
getBusinessWireAccountInstructions wireAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/wires/" <> UUID.toText wireAccountId <> "/instructions"
    params = Params Nothing []

---------------------------------------------------------------
-- /businessAccount/balances endpoint
---------------------------------------------------------------

-- | List all business balances
-- https://developers.circle.com/reference/listbusinesspayouts
listAllBusinessBalances :: CircleAPIRequest BalanceRequest TupleBS8 BSL.ByteString
listAllBusinessBalances = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/balances"
    params = Params Nothing []

---------------------------------------------------------------
-- /configuration endpoint
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
-- /encryption/public endpoint
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
-- /channels endpoint
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
-- /stablecoins endpoint
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
-- /notifications/subscriptions endpoint
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
createSubscription :: SubscriptionRequestBody -> CircleAPIRequest SubscriptionRequest TupleBS8 BSL.ByteString
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
    url = "notifications/subscriptions" <> UUID.toText resourceId
    params = Params Nothing []

---------------------------------------------------------------
-- /businessAccount/payouts endpoint
---------------------------------------------------------------

-- | Lists all payouts made from a given business account
-- https://developers.circle.com/reference/listbusinesspayouts
listAllBusinessAccountPayouts :: CircleAPIRequest PayoutsRequest TupleBS8 BSL.ByteString
listAllBusinessAccountPayouts = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/payouts"
    params = Params Nothing []

-- | Gets a specific payout associated with a business account
-- https://developers.circle.com/reference/getbusinesspayout
getBusinessAccountPayout :: UUID -> CircleAPIRequest PayoutRequest TupleBS8 BSL.ByteString
getBusinessAccountPayout payoutId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/payouts" <> UUID.toText payoutId
    params = Params Nothing []

-- | Creates a business account payout
-- https://developers.circle.com/reference/createbusinesspayout
createBusinessAccountPayout :: BusinessPayoutRequestBody -> CircleAPIRequest PayoutRequest TupleBS8 BSL.ByteString
createBusinessAccountPayout payoutBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/payouts"
    params = Params (Just $ Body (encode payoutBody)) []

---------------------------------------------------------------
-- /businessAccount/transfers endpoint
---------------------------------------------------------------

-- | Searches for transfers from your business account.
-- If the date parameters are omitted, returns the most recent transfers.
-- This endpoint returns up to 50 transfers in descending chronological order or pageSize, if provided.
-- https://developers.circle.com/reference/listbusinesstransfers
listAllBusinessAccountTransfers :: CircleAPIRequest TransfersRequest TupleBS8 BSL.ByteString
listAllBusinessAccountTransfers = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/transfers"
    params = Params Nothing []

-- | Get a business account transfer based on a transfer ID
-- https://developers.circle.com/reference/getbusinesstransfer
getBusinessAccountTransfer :: UUID -> CircleAPIRequest TransferRequest TupleBS8 BSL.ByteString
getBusinessAccountTransfer transferId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/transfers/" <> T.pack (show transferId)
    params = Params Nothing []

-- | Create a new transfer
-- https://developers.circle.com/reference/createbusinesstransfer
createBusinessAccountTransfer :: BusinessTransferRequestBody -> CircleAPIRequest TransferRequest TupleBS8 BSL.ByteString
createBusinessAccountTransfer transferBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/transfers"
    params = Params (Just $ Body (encode transferBody)) []

---------------------------------------------------------------
-- /businessAccount/wallets/addresses endpoint
---------------------------------------------------------------

-- | List all deposit addresses
-- https://developers.circle.com/developer/reference/getbusinessdepositaddress
listAllBusinessAccountDepositAddresses :: CircleAPIRequest DepositAddressesRequest TupleBS8 BSL.ByteString
listAllBusinessAccountDepositAddresses = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/wallets/addresses/deposit"
    params = Params Nothing []

-- | Create new business account deposit address
-- Generates a new blockchain address for a wallet for a given currency/chain pair.
-- Circle may reuse addresses on blockchains that support reuse.
-- For example, if you're requesting two addresses for depositing USD and ETH, both on Ethereum,
-- you may see the same Ethereum address returned.
-- Depositing cryptocurrency to a generated address will credit the associated wallet with the value of the deposit.
-- https://developers.circle.com/developer/reference/createbusinessdepositaddress
createBusinessAccountDepositAddress :: DepositAddressRequestBody -> CircleAPIRequest DepositAddressRequest TupleBS8 BSL.ByteString
createBusinessAccountDepositAddress depositAddressBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/wallets/addresses/deposit"
    params = Params (Just $ Body (encode depositAddressBody)) []

-- | List all recipient addresses
-- Returns a list of recipient addresses that have each been verified and are eligible for transfers.
-- Any recipient addresses pending verification are not included in the response.
-- https://developers.circle.com/developer/reference/listbusinessrecipientaddresses
listAllBusinessAccountRecipientAddresses :: CircleAPIRequest RecipientAddressesRequest TupleBS8 BSL.ByteString
listAllBusinessAccountRecipientAddresses = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/wallets/addresses/recipient"
    params = Params Nothing []

-- | Create a new recipient address
-- Stores an external blockchain address. Once added, the recipient address must be verified to ensure that you know and trust each new address.
-- https://developers.circle.com/developer/reference/createbusinessrecipientaddress
createBusinessAccountRecipientAddress :: RecipientAddressRequestBody -> CircleAPIRequest RecipientAddressRequest TupleBS8 BSL.ByteString
createBusinessAccountRecipientAddress recipientAddressBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/wallets/addresses/recipient"
    params = Params (Just $ Body (encode recipientAddressBody)) []

---------------------------------------------------------------
-- /businessAccount/deposits Endpoint
---------------------------------------------------------------

-- | List all deposits
-- Searches for deposits sent to your business account. If the date parameters are omitted, returns the most recent deposits.
-- This endpoint returns up to 50 deposits in descending chronological order or pageSize, if provided.
-- https://developers.circle.com/developer/reference/listbusinessdeposits
listAllBusinessAccountDeposits :: CircleAPIRequest DepositsRequest TupleBS8 BSL.ByteString
listAllBusinessAccountDeposits = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/deposits"
    params = Params Nothing []

---------------------------------------------------------------
-- /businessAccount/banks/signet endpoint (Production-Only)
---------------------------------------------------------------

-- TODO how to test these if no sandbox instance?

-- | Create a signet bank account
-- https://developers.circle.com/developer/reference/createbusinesssignetaccount
createSignetBankAccount :: SignetBankAccountRequestBody -> CircleAPIRequest SignetBankAccountRequest TupleBS8 BSL.ByteString
createSignetBankAccount signetBankAccountBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "businessAccount/banks/signet"
    params = Params (Just $ Body (encode signetBankAccountBody)) []

-- | Get a list of Signet accounts
-- https://developers.circle.com/developer/reference/listbusinesssignetaccounts
listSignetAccounts :: CircleAPIRequest SignetBankAccountsRequest TupleBS8 BSL.ByteString
listSignetAccounts = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/signet"
    params = Params Nothing []

-- | Get a single Signet bank account, accepts the Signet bank account Id as a parameter
-- https://developers.circle.com/developer/reference/getbusinesssignetaccount
getSignetAccount :: UUID -> CircleAPIRequest SignetBankAccountRequestBody TupleBS8 BSL.ByteString
getSignetAccount signetBankAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/signet/" <> UUID.toText signetBankAccountId
    params = Params Nothing []

-- Get the Signet transfer instructions into the Circle bank account given your bank account id (only available on Production now).
-- https://developers.circle.com/developer/reference/getbusinesssignetaccountinstructions
getSignetAccountInstructions :: UUID -> CircleAPIRequest SignetBankInstructionsResponseData TupleBS8 BSL.ByteString
getSignetAccountInstructions signetBankAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/signet/" <> UUID.toText signetBankAccountId <> "/instructions"
    params = Params Nothing []


---------------------------------------------------------------
-- /businessAccount/banks/sen endpoint (Silvergate SEN operations)
---------------------------------------------------------------

-- | Create a bank account for a SEN
-- https://developers.circle.com/developer/reference/createbusinesssenaccount
createSENAccount :: SENAccountRequestBody -> CircleAPIRequest SENAccountRequest TupleBS8 BSL.ByteString
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
    url = "businessAccount/banks/sen/" <> UUID.toText senAccountId
    params = Params Nothing []

-- | Get the SEN transfer instructions into the Circle bank account given your bank account id.
-- https://developers.circle.com/developer/reference/getbusinesssenaccountinstructions
getSENAccountInstructions :: UUID -> CircleAPIRequest SENInstructionsRequest TupleBS8 BSL.ByteString
getSENAccountInstructions senAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "businessAccount/banks/sen/" <> UUID.toText senAccountId <> "/instructions"
    params = Params Nothing []

---------------------------------------------------------------
-- /payments endpoint
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
createPayment :: CreatePaymentRequestBody -> CircleAPIRequest PaymentRequest TupleBS8 BSL.ByteString
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
    url = "payments/" <> UUID.toText paymentId
    params = Params Nothing []

-- | Cancel a fiat payment
-- https://developers.circle.com/developer/reference/payments-payments-cancel-id
cancelPayment :: UUID -> CancelPaymentRequestBody -> CircleAPIRequest PaymentRequest TupleBS8 BSL.ByteString
cancelPayment paymentId cancelPaymentBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "payments/" <> UUID.toText paymentId <> "/cancel"
    params = Params (Just $ Body (encode cancelPaymentBody)) []

-- | Refund a fiat payment
-- https://developers.circle.com/developer/reference/payments-payments-refund-id
refundPayment :: UUID -> RefundPaymentRequestBody -> CircleAPIRequest PaymentRequest TupleBS8 BSL.ByteString
refundPayment paymentId refundPaymentBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "payments/" <> UUID.toText paymentId <> "/refund"
    params = Params (Just $ Body (encode refundPaymentBody)) []

-- TODO add capture payment method?

---------------------------------------------------------------
-- /mock/payments endpoints
---------------------------------------------------------------

-- TODO constrain these method to run in the sandbox only.  Would be cool to do the same thing with the Production only methods

-- | Create mock wire payment SANDBOX ONLY
-- In the sandbox environment, initiate a mock wire payment that mimics the behavior of funds sent through the bank (wire) account linked to master wallet.
-- https://developers.circle.com/developer/reference/createmockwirepayment
createMockWirePayment :: MockSenOrWirePaymentRequestBody -> CircleAPIRequest MockPaymentRequest TupleBS8 BSL.ByteString
createMockWirePayment wireBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "mocks/payments/wire"
    params = Params (Just $ Body (encode wireBody)) []

-- | Create mock SEPA payment SANDBOX ONLY (in Beta)
-- In the sandbox environment, initiate a mock SEPA payment that mimics the behavior of funds sent through the bank (SEPA) account linked to master wallet.
-- https://developers.circle.com/developer/reference/createmocksepapayment
createMockSEPAPayment :: MockSEPAPaymentRequestBody -> CircleAPIRequest MockPaymentRequest TupleBS8 BSL.ByteString
createMockSEPAPayment sepaBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "mocks/payments/sepa"
    params = Params (Just $ Body (encode sepaBody)) []

-- | Create mock Silvergate payment SANDBOX ONLY
-- In the sandbox environment, initiate a mock SEN transfer that mimics the behavior of funds sent through the Silvergate SEN account linked to master wallet.
-- https://developers.circle.com/developer/reference/createmocksenpayment
createMockSilvergatePayment :: MockSenOrWirePaymentRequestBody -> CircleAPIRequest MockPaymentRequest TupleBS8 BSL.ByteString
createMockSilvergatePayment senBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "mocks/payments/sen"
    params = Params (Just $ Body (encode senBody)) []

---------------------------------------------------------------
-- /mock/ach endpoint
---------------------------------------------------------------

-- | Create mock ACH account SANDBOX ONLY
-- In the sandbox environment, create a mock ACH account and retrieve a processor token that can be used to link an ACH account.
-- https://developers.circle.com/developer/reference/createmockachaccount-1
createMockACHBankAccount :: CreateMockACHBankAccountRequestBody -> CircleAPIRequest MockAccountRequest TupleBS8 BSL.ByteString
createMockACHBankAccount achPaymentBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "mocks/ach/account"
    params = Params (Just $ Body (encode achPaymentBody)) []

---------------------------------------------------------------
-- /mock/cards/chargebacks endpoint
---------------------------------------------------------------

-- | Create mock chargeback SANDBOX ONLY
-- In the sandbox environment, initiate a mock chargeback of a specified payment. 
-- The entire payment will be charged back for its full value. The payment must be in the paid state 
-- (otherwise the endpoint will return a 404), and each payment can only be charged back once 
-- (otherwise the endpoint will return a 409). This endpoint is only available in the sandbox environment.
-- https://developers.circle.com/developer/reference/payments-chargebacks-mock-create
createMockChargeback :: UUID -> CircleAPIRequest MockChargebackRequest TupleBS8 BSL.ByteString
createMockChargeback paymentId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "mocks/cards/chargebacks"
    params = Params (Just $ Body (encode paymentId)) []

---------------------------------------------------------------
-- /transfers endpoint (On-chain transfers)
---------------------------------------------------------------

-- | Searches for transfers.
-- Searches for transfers involving the provided wallets. If no wallet ids
-- are provided, searches all wallets associated with your Circle API
-- account. If the date parameters are omitted, returns the most recent
-- transfers. This endpoint returns up to 50 transfers in descending
-- chronological order or pageSize, if provided.
-- https://developers.circle.com/developer/reference/listtransfers-1
listAllOnChainTransfers :: CircleAPIRequest OnChainTransfersRequest TupleBS8 BSL.ByteString
listAllOnChainTransfers = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "transfers"
    params = Params Nothing []

-- | Get a business account transfer based on a transfer ID
-- https://developers.circle.com/developer/reference/gettransfer
getOnChainTransfer :: UUID -> CircleAPIRequest TransferRequest TupleBS8 BSL.ByteString
getOnChainTransfer transferId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "transfers/" <> UUID.toText transferId
    params = Params Nothing []

-- | Create an on-chain transfer (i.e. a crypto payment)
-- https://developers.circle.com/developer/reference/accounts-transfers-create
createOnChainTransfer :: OnChainTransferRequestBody -> CircleAPIRequest TransferRequest TupleBS8 BSL.ByteString
createOnChainTransfer onChainTransferBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "transfers"
    params = Params (Just $ Body (encode onChainTransferBody)) []

---------------------------------------------------------------
-- /cards endpoint
---------------------------------------------------------------

-- | List all cards
-- https://developers.circle.com/developer/reference/listcards
listAllCards :: CircleAPIRequest CardsRequest TupleBS8 BSL.ByteString
listAllCards = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "cards"
    params = Params Nothing []

-- | Get a card
-- https://developers.circle.com/developer/reference/payments-cards-get-id
getCard :: UUID -> CircleAPIRequest CardRequest TupleBS8 BSL.ByteString
getCard cardId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "cards/" <> UUID.toText cardId
    params = Params Nothing []

-- | Create a card
-- https://developers.circle.com/developer/reference/payments-cards-create
createCard :: CreateCardRequestBody -> CircleAPIRequest CardRequest TupleBS8 BSL.ByteString
createCard createCardBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "cards"
    params = Params (Just $ Body (encode createCardBody)) []

-- | Update a card
-- https://developers.circle.com/developer/reference/updatecard
updateCard :: UUID -> UpdateCardRequestBody -> CircleAPIRequest CardRequest TupleBS8 BSL.ByteString
updateCard cardId updateCardBody = do
  mkCircleAPIRequest NHTM.methodPut url params
  where
    url = "cards/" <> UUID.toText cardId
    params = Params (Just $ Body (encode updateCardBody)) []

---------------------------------------------------------------
-- banks/wires endpoint
---------------------------------------------------------------

-- | Create a bank account for a wire
-- https://developers.circle.com/developer/reference/createwireaccount
createWireAccount :: WireAccountRequestBody -> CircleAPIRequest WireAccountRequest TupleBS8 BSL.ByteString
createWireAccount wireAccountBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "banks/wires"
    params = case wireAccountBody of
      USBankAccount usBankAccountBody -> Params (Just $ Body (encode usBankAccountBody)) []
      IBANBankAccount ibanBankAccountBody -> Params (Just $ Body (encode ibanBankAccountBody)) []
      NonIBANBankAccount nonIBANBankAccountBody -> Params (Just $ Body (encode nonIBANBankAccountBody)) []

-- | Get a single wire account, accepts the wire account Id as a parameter
-- https://developers.circle.com/developer/reference/getwireaccount-1
getWireAccount :: UUID -> CircleAPIRequest WireAccountRequest TupleBS8 BSL.ByteString
getWireAccount wireAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "banks/wires/" <> UUID.toText wireAccountId
    params = Params Nothing []

-- | Get the wire transfer instructions into the Circle bank account given your bank account id.
-- https://developers.circle.com/developer/reference/getwireaccountinstructions
getWireAccountInstructions :: UUID -> CircleAPIRequest WireInstructionsRequest TupleBS8 BSL.ByteString
getWireAccountInstructions wireAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "banks/wires/" <> UUID.toText wireAccountId <> "/instructions"
    params = Params Nothing []

---------------------------------------------------------------
-- /banks/ach endpoint
---------------------------------------------------------------

-- NB: Use createMockACHBankAccount in the sandbox environment create a mock ACH account and retrieve a processor token that can be used to link an ACH account.

-- | Create an ACH account
-- https://developers.circle.com/developer/reference/payments-bank-accounts-ach-mock
createACHAccount :: CreateACHBankAccountRequestBody -> CircleAPIRequest ACHBankAccountRequest TupleBS8 BSL.ByteString
createACHAccount achAccountBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "banks/ach"
    params = Params (Just $ Body (encode achAccountBody)) []

-- | Get an ACH account
-- https://developers.circle.com/developer/reference/getachaccount-1
getACHAccount :: UUID -> CircleAPIRequest ACHBankAccountRequest TupleBS8 BSL.ByteString
getACHAccount achAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "banks/ach/" <> UUID.toText achAccountId
    params = Params Nothing []

---------------------------------------------------------------
-- /banks/sepa endpoint
---------------------------------------------------------------

-- TODO some way to mark this stuff as being in beta?  Is it even worth it?

-- | Create a SEPA account (in beta)
-- https://developers.circle.com/developer/reference/createsepaaccount-1
createSEPAAccount :: SEPAAccountRequestBody -> CircleAPIRequest SEPAAccountRequest TupleBS8 BSL.ByteString
createSEPAAccount sepaAccountBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "banks/sepa"
    params = Params (Just $ Body (encode sepaAccountBody)) []

-- | Get a SEPA account (in beta)
-- https://developers.circle.com/developer/reference/getsepaaccount-1
getSEPAAccount :: UUID -> CircleAPIRequest SEPAAccountRequest TupleBS8 BSL.ByteString
getSEPAAccount sepaAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "banks/sepa/" <> UUID.toText sepaAccountId
    params = Params Nothing []

-- | Get instructions for a SEPA transfer (in beta)
-- https://developers.circle.com/developer/reference/getsepaaccountinstructions
getSEPAAccountInstructions :: UUID -> CircleAPIRequest SEPAInstructionsRequest TupleBS8 BSL.ByteString
getSEPAAccountInstructions sepaAccountId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "banks/sepa/" <> UUID.toText sepaAccountId <> "/instructions"
    params = Params Nothing []

---------------------------------------------------------------
-- /settlements endpoint
---------------------------------------------------------------

-- | List all settlements
-- https://developers.circle.com/developer/reference/listsettlements
listAllSettlements :: CircleAPIRequest SettlementsRequest TupleBS8 BSL.ByteString
listAllSettlements = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "settlements"
    params = Params Nothing []

-- | Get a settlement
-- https://developers.circle.com/developer/reference/payments-settlements-get-id
getSettlement :: UUID -> CircleAPIRequest SettlementRequest TupleBS8 BSL.ByteString
getSettlement settlementId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "settlements/" <> UUID.toText settlementId
    params = Params Nothing []

---------------------------------------------------------------
-- /chargebacks endpoint
---------------------------------------------------------------

-- List all chargebacks
-- https://developers.circle.com/developer/reference/listchargebacks
listAllChargebacks :: CircleAPIRequest ChargebacksRequest TupleBS8 BSL.ByteString
listAllChargebacks = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "chargebacks"
    params = Params Nothing []

-- | Get a chargeback
-- https://developers.circle.com/developer/reference/payments-chargebacks-get-id
getChargeback :: UUID -> CircleAPIRequest ChargebackRequest TupleBS8 BSL.ByteString
getChargeback chargebackId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "chargebacks/" <> UUID.toText chargebackId
    params = Params Nothing []

---------------------------------------------------------------
-- /reversals endpoint
---------------------------------------------------------------

-- | Retrieve a list of ACH payment reversals. Results will be sorted by create date descending; more recent reversals will be at the beginning of the list
-- https://developers.circle.com/developer/reference/listreversals
listAllACHReversals :: CircleAPIRequest ReversalsRequest TupleBS8 BSL.ByteString
listAllACHReversals = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "reversals"
    params = Params Nothing []

---------------------------------------------------------------
-- /balances endpoint
---------------------------------------------------------------

-- | Retrieves the balance of merchant funds that have settled and also of funds that have been sent for processing but have not yet settled.
-- https://developers.circle.com/developer/reference/listbalances
listAllBalances :: CircleAPIRequest BalanceRequest TupleBS8 BSL.ByteString
listAllBalances = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "balances"
    params = Params Nothing []

---------------------------------------------------------------
-- /paymentIntents endpoint
---------------------------------------------------------------

-- | List all payment intents
-- https://developers.circle.com/developer/reference/listpaymentintents
listAllPaymentIntents :: CircleAPIRequest PaymentIntentsRequest TupleBS8 BSL.ByteString
listAllPaymentIntents = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "paymentIntents"
    params = Params Nothing []

-- | Create a payment intent
-- https://developers.circle.com/developer/reference/createpaymentintent
createPaymentIntent :: CreatePaymentIntentRequestBody -> CircleAPIRequest PaymentIntentRequest TupleBS8 BSL.ByteString
createPaymentIntent createPaymentBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "paymentIntents"
    params = Params (Just $ Body (encode createPaymentBody)) []

-- | Get a payment intent
-- https://developers.circle.com/developer/reference/getpaymentintent
getPaymentIntent :: UUID -> CircleAPIRequest PaymentIntentRequest TupleBS8 BSL.ByteString
getPaymentIntent paymentIntentId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "paymentIntents/" <> UUID.toText paymentIntentId
    params = Params Nothing []

-- | Expire a payment intent
-- https://developers.circle.com/developer/reference/expirepaymentintent
expirePaymentIntent :: UUID -> CircleAPIRequest PaymentIntentRequest TupleBS8 BSL.ByteString
expirePaymentIntent paymentIntentId = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "paymentIntents/" <> UUID.toText paymentIntentId <> "/expire"
    params = Params Nothing []

---------------------------------------------------------------
-- /payouts endpoint
---------------------------------------------------------------

-- | Lists all payouts made from a given account
-- https://developers.circle.com/developer/reference/listpayouts
listAllPayouts :: CircleAPIRequest PayoutsRequest TupleBS8 BSL.ByteString
listAllPayouts = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "payouts"
    params = Params Nothing []

-- | Gets a specific payout based on an ID
-- https://developers.circle.com/developer/reference/payouts-payouts-get-id
getPayout :: UUID -> CircleAPIRequest PayoutRequest TupleBS8 BSL.ByteString
getPayout payoutId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "payouts" <> UUID.toText payoutId
    params = Params Nothing []

-- | Creates a payout
-- https://developers.circle.com/developer/reference/payouts-payouts-create
createPayout :: PayoutRequestBody -> CircleAPIRequest PayoutRequest TupleBS8 BSL.ByteString
createPayout payoutBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "payouts"
    params = Params (Just $ Body (encode payoutBody)) []

---------------------------------------------------------------
-- /transfers endpoint (On-chain transfers)
---------------------------------------------------------------

-- | Searches for transfers from your account.
-- If the date parameters are omitted, returns the most recent transfers.
-- This endpoint returns up to 50 transfers in descending chronological order or pageSize, if provided.
-- https://developers.circle.com/developer/reference/listtransfers
listAllTransfers :: CircleAPIRequest TransfersRequest TupleBS8 BSL.ByteString
listAllTransfers = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "transfers"
    params = Params Nothing []

-- | Get a transfer based on a transfer ID
-- https://developers.circle.com/reference/getbusinesstransfer
getTransfer :: UUID -> CircleAPIRequest TransferRequest TupleBS8 BSL.ByteString
getTransfer transferId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "transfers/" <> UUID.toText transferId
    params = Params Nothing []

-- | Create a new transfer
-- https://developers.circle.com/developer/reference/payouts-transfers-create
createTransfer :: TransferRequestBody -> CircleAPIRequest TransferRequest TupleBS8 BSL.ByteString
createTransfer transferBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "transfers"
    params = Params (Just $ Body (encode transferBody)) []

---------------------------------------------------------------
-- /returns endpoint
---------------------------------------------------------------

-- | Retrieve a list of Wire and ACH payout returns. Results will be sorted by create date descending; 
-- more recent returns will be at the beginning of the list.
-- https://developers.circle.com/developer/reference/listreturns
listAllReturns :: CircleAPIRequest ReturnsRequest TupleBS8 BSL.ByteString
listAllReturns = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "returns"
    params = Params Nothing []

---------------------------------------------------------------
-- /wallets endpoint
---------------------------------------------------------------

-- | Retrieves a list of a user's wallets.
-- https://developers.circle.com/developer/reference/listwallets
listAllWallets :: CircleAPIRequest WalletsRequest TupleBS8 BSL.ByteString
listAllWallets = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "wallets"
    params = Params Nothing []

-- | Get a wallet
-- https://developers.circle.com/developer/reference/accounts-wallets-get-id
getWallet :: UUID -> CircleAPIRequest WalletRequest TupleBS8 BSL.ByteString
getWallet walletId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "wallets/" <> UUID.toText walletId
    params = Params Nothing []

-- | Creates an end user wallet.
-- https://developers.circle.com/developer/reference/accounts-wallets-create
createWallet :: CreateWalletRequestBody -> CircleAPIRequest WalletRequest TupleBS8 BSL.ByteString
createWallet walletBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "wallets"
    params = Params (Just $ Body (encode walletBody)) []

-- | Create new blockchain deposit address
-- Generates a new blockchain address for a wallet for a given currency/chain pair.
-- Circle may reuse addresses on blockchains that support reuse.
-- For example, if you're requesting two addresses for depositing USD and ETH, both on Ethereum,
-- you may see the same Ethereum address returned.
-- Depositing cryptocurrency to a generated address will credit the associated wallet with the value of the deposit.
-- https://developers.circle.com/developer/reference/payments-on-chain-addresses-create
createDepositAddress :: UUID -> DepositAddressRequestBody -> CircleAPIRequest DepositAddressRequest TupleBS8 BSL.ByteString
createDepositAddress walletId depositAddressBody = do
  mkCircleAPIRequest NHTM.methodPost url params
  where
    url = "wallets/" <> UUID.toText walletId <> "/addresses"
    params = Params (Just $ Body (encode depositAddressBody)) []

-- | List all recipient addresses associated with a wallet Id
-- Retrieves a list of addresses associated with a wallet.
-- https://developers.circle.com/developer/reference/listaddresses
listAllAddresses :: UUID -> CircleAPIRequest RecipientAddressesRequest TupleBS8 BSL.ByteString
listAllAddresses walletId = do
  mkCircleAPIRequest NHTM.methodGet url params
  where
    url = "wallets/" <> UUID.toText walletId <> "/addresses"
    params = Params Nothing []

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
    Left s -> return (Left (CircleError (T.pack s) response))
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
  liftIO $ print request
  response <- circleTest' config request tlsManager
  liftIO $ print response
  let result = eitherDecode $ responseBody response
  case result of
    Left s -> return (Left (CircleError (T.pack s) response))
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