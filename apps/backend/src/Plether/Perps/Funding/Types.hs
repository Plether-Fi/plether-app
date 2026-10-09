-- | Provider-independent funding bindings. Amounts are base-unit decimal strings.
module Plether.Perps.Funding.Types
  ( FundingDeployment (..), QuoteRequest (..), SourceTransaction (..)
  , ProviderQuote (..), FundingProvider (..)
  , validateFundingReleaseBinding, deploymentReadinessKey, loadFundingDeployment, validateAddress, validateAmount, validateHash
  , fieldText, fieldInteger, setFields, publicIntent, publicObservedIntent, newIdentifier, intentFromQuote
  ) where

import Crypto.Random (getRandomBytes)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Plether.Ethereum.Abi (keccak256)
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- Immutable release bindings, validated again on chain before quoting and observing.
data FundingDeployment = FundingDeployment
  { fdChainId :: Integer, fdReleaseId :: Text, fdClearinghouse :: Text
  , fdToken :: Text, fdDestinationSpokePool :: Text, fdDestinationSpokePoolCodeHash :: Text
  , fdDestinationSpokePoolImplementation :: Text, fdDestinationSpokePoolImplementationCodeHash :: Text
  , fdMulticallHandler :: Text, fdMulticallHandlerCodeHash :: Text
  , fdConfirmations :: Integer, fdStartBlock :: Integer, fdClearinghouseCodeHash :: Text
  } deriving stock (Eq, Show)
instance FromJSON FundingDeployment where
  parseJSON = withObject "funding deployment" $ \v -> do
    chain <- v .: "destinationChainId"
    release <- v .: "releaseId"
    ch <- v .: "clearinghouse" >>= validAddress
    token <- v .: "token" >>= validAddress
    spokePool <- v .: "destinationSpokePool" >>= validAddress
    spokePoolHash <- v .: "destinationSpokePoolCodeHash" >>= validHash
    implementation <- v .: "destinationSpokePoolImplementation" >>= validAddress
    implementationHash <- v .: "destinationSpokePoolImplementationCodeHash" >>= validHash
    handler <- v .: "multicallHandler" >>= validAddress
    handlerHash <- v .: "multicallHandlerCodeHash" >>= validHash
    confirmations <- v .: "confirmations"
    start <- v .: "startBlock"
    clearinghouseCodeHash <- v .: "clearinghouseCodeHash" >>= validHash
    if chain <= 0 || T.null release || T.length release > 128 || confirmations < 1 || confirmations > 1000 || start < 0
      then fail "Invalid funding deployment bounds"
      else pure $ FundingDeployment chain release ch token spokePool spokePoolHash implementation implementationHash handler handlerHash confirmations start clearinghouseCodeHash
instance ToJSON FundingDeployment where
  toJSON FundingDeployment {..} = object
    ["destinationChainId" .= fdChainId, "releaseId" .= fdReleaseId
    ,"clearinghouse" .= fdClearinghouse, "token" .= fdToken
    ,"destinationSpokePool" .= fdDestinationSpokePool,"destinationSpokePoolCodeHash" .= fdDestinationSpokePoolCodeHash
    ,"destinationSpokePoolImplementation" .= fdDestinationSpokePoolImplementation
    ,"destinationSpokePoolImplementationCodeHash" .= fdDestinationSpokePoolImplementationCodeHash
    ,"multicallHandler" .= fdMulticallHandler,"multicallHandlerCodeHash" .= fdMulticallHandlerCodeHash
    ,"clearinghouseCodeHash" .= fdClearinghouseCodeHash, "confirmations" .= fdConfirmations, "startBlock" .= fdStartBlock]

validateFundingReleaseBinding :: Integer -> Text -> Text -> FundingDeployment -> Either Text FundingDeployment
validateFundingReleaseBinding chain clearinghouse token deployment
  | fdChainId deployment == chain && fdClearinghouse deployment == T.toLower clearinghouse && fdToken deployment == T.toLower token = Right deployment
  | otherwise = Left "FUNDING_APPLICATION_RELEASE_MISMATCH"

-- A reused release label must not borrow another deployment's ready heartbeat.
deploymentReadinessKey :: FundingDeployment -> Text
deploymentReadinessKey deployment = fdReleaseId deployment <> ":" <>
  TE.decodeUtf8 (B16.encode $ keccak256 $ LBS.toStrict $ encode deployment)

loadFundingDeployment :: IO (Either Text (Maybe FundingDeployment))
loadFundingDeployment = lookupEnv "PERPS_FUNDING_DEPLOYMENT_JSON" >>= \case
  Nothing -> pure $ Right Nothing
  Just raw -> pure $ either (const $ Left "Invalid PERPS_FUNDING_DEPLOYMENT_JSON") (Right . Just)
    (eitherDecodeStrict' $ TE.encodeUtf8 $ T.pack raw)

data QuoteRequest = QuoteRequest
  { qrBeneficiary :: Text, qrSourceOwner :: Text, qrSourceChainId :: Integer, qrSourceToken :: Text, qrSourceAmount :: Text
  } deriving stock (Eq, Show)
instance FromJSON QuoteRequest where
  parseJSON = withObject "funding quote request" $ \v -> do
    beneficiary <- v .: "beneficiary" >>= validAddress
    owner <- v .: "ownerAddress" >>= validAddress
    chain <- v .: "sourceChainId"
    token <- v .: "sourceToken" >>= validAddress
    amount <- v .: "sourceAmount"
    either (fail . T.unpack) (const $ pure ()) $ validateAmount amount
    if chain <= 0 then fail "Invalid source chain" else pure $ QuoteRequest beneficiary owner chain token amount

data SourceTransaction = SourceTransaction
  { stChainId :: Integer, stKind :: Text, stTo :: Text, stData :: Text, stValue :: Text
  } deriving stock (Eq, Show)
instance ToJSON SourceTransaction where
  toJSON SourceTransaction {..} = object ["chainId" .= stChainId,"kind" .= stKind,"to" .= stTo,"data" .= stData,"value" .= stValue]

data ProviderQuote = ProviderQuote
  { pqExpiresAt :: Integer, pqEstimatedAmount :: Text, pqMinimumAmount :: Text
  , pqTransactions :: [SourceTransaction], pqProviderReference :: Text, pqDestinationMessage :: Text
  } deriving stock (Eq, Show)

-- A provider must produce an executable source-wallet route whose destination
-- is exactly the pinned clearinghouse/beneficiary/token. There is no generic passthrough URL.
data FundingProvider = FundingProvider
  { fpName :: Text, fpUnavailableReason :: Maybe Text
  , fpQuote :: FundingDeployment -> QuoteRequest -> Text -> IO (Either Text ProviderQuote)
  }

validateAddress :: Text -> Either Text Text
validateAddress value
  | T.length value == 42 && T.isPrefixOf "0x" value && T.all isHexDigit (T.drop 2 value)
      && T.any (/= '0') (T.drop 2 value) = Right $ T.toLower value
  | otherwise = Left "Invalid nonzero address"
validateHash :: Text -> Either Text Text
validateHash value
  | T.length value == 66 && T.isPrefixOf "0x" value && T.all isHexDigit (T.drop 2 value) = Right $ T.toLower value
  | otherwise = Left "Invalid bytes32"
validateAmount :: Text -> Either Text Integer
validateAmount value
  | not (T.null value) && T.length value <= 78 && T.head value /= '0' && T.all (\c -> c >= '0' && c <= '9') value
  , Just amount <- readMaybe (T.unpack value), amount > 0, amount < 2 ^ (256 :: Int) = Right amount
  | otherwise = Left "Invalid positive uint256 amount"
validAddress, validHash :: Text -> Parser Text
validAddress = either (fail . T.unpack) pure . validateAddress
validHash = either (fail . T.unpack) pure . validateHash
fieldText :: Text -> Value -> Maybe Text
fieldText key = parseMaybe $ withObject "object" $ \v -> v .: Key.fromText key
fieldInteger :: Text -> Value -> Maybe Integer
fieldInteger key = parseMaybe $ withObject "object" $ \v -> v .: Key.fromText key
setFields :: [(Text, Value)] -> Value -> Value
setFields fields (Object value) = Object $ foldr (\(key,item) -> KM.insert (Key.fromText key) item) value fields
setFields _ value = value
publicIntent :: Value -> Value
publicIntent (Object value) = Object $ foldr KM.delete value
  ["providerReference"]
publicIntent value = value
-- Every intent endpoint applies the same freshness rule, including idempotent
-- create/source/retry responses. Historical proof cannot advertise live credit.
publicObservedIntent :: Integer -> Bool -> Value -> Value
publicObservedIntent now observerReady intent = publicIntent $
  if stale && fieldText "status" intent `elem` [Just "confirmed",Just "needs-deposit"]
    then setFields [("status",String "bridging"),("creditedAmount",String "0")
      ,("lastError",String "DESTINATION_OBSERVER_UNAVAILABLE")] intent
    else intent
  where
    stale = not observerReady || maybe True (\checked -> checked > now || now-checked > 60) (fieldInteger "lastCheckedAt" intent)
newIdentifier :: IO Text
newIdentifier = do
  entropy <- getRandomBytes 32 :: IO ByteString
  pure $ "0x" <> TE.decodeUtf8 (B16.encode entropy)
intentFromQuote :: Text -> Value -> Value
intentFromQuote identifier = setFields [("intentId", String identifier),("status",String "awaiting-source")]
