module Plether.AA.Preparation
  ( PreparationIntent (..), parsePreparationIntent, unsignedSkeleton, intentHash, matchesIntent, internalRequest
  , PreparationLocator (..), parsePreparationLocator, preparationStatusResponse
  , gasPolicyVersion, sepoliaExecutionGasCap, executionGasWithHeadroom ) where

import Control.Monad (unless)
import Data.Aeson (Value (..), object, encode, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isHexDigit)
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Plether.AA.Pimlico as Legacy
import Plether.Ethereum.Abi (keccak256)

data PreparationIntent = PreparationIntent
  { piIdentifier :: Text, piSender :: Text, piCallData :: Text
  , piFactory :: Maybe Text, piFactoryData :: Maybe Text, piResumeOnly :: Bool
  } deriving stock (Eq, Show)

-- Applied ONLY to a fresh Alto estimate, before immutable preparation storage.
-- Changing this identifier requires a new reviewed preparation, never a rewrite
-- of a persisted/signed operation or its reservation.
gasPolicyVersion :: Text
gasPolicyVersion = "execution-headroom-v3-sepolia-cap3000000-150pct-min100000"

-- Native issuance is restricted to Arbitrum Sepolia. All action classes share
-- this ceiling; the separate wei liability and verification-gas caps do not move.
sepoliaExecutionGasCap :: Integer
sepoliaExecutionGasCap = 3_000_000

executionGasWithHeadroom :: Integer -> Either Text Integer
executionGasWithHeadroom estimated = do
  unless (estimated > 0 && estimated <= sepoliaExecutionGasCap) $
    Left "Invalid execution gas estimate"
  let padded = max ((estimated * 3 + 1) `div` 2) (estimated + 100_000)
  unless (padded <= sepoliaExecutionGasCap) $
    Left "Execution gas including headroom exceeds the reviewed sponsorship bounds"
  pure padded

parsePreparationIntent :: [Value] -> Either Legacy.ProxyFailure PreparationIntent
parsePreparationIntent [Object fields] = do
  unless (all (`elem` ["version","preparationId","chainId","entryPoint","sender","callData","factory","factoryData","resumeOnly"]) $ KM.keys fields) $
    Left $ Legacy.invalidParams "Preparation contains unsupported fields"
  unless (KM.lookup "version" fields == Just (Number 1)
    && KM.lookup "chainId" fields == Just (String "0x66eee")
    && fmap lower (KM.lookup "entryPoint" fields) == Just (String entryPoint)) $
    Left $ Legacy.invalidParams "Unsupported preparation version, chain or EntryPoint"
  resumeOnly <- case KM.lookup "resumeOnly" fields of
    Nothing -> Right False
    Just (Bool value) -> Right value
    _ -> Left $ Legacy.invalidParams "resumeOnly must be boolean"
  identifier <- requiredHex fields "preparationId" (Just 32)
  sender <- requiredHex fields "sender" (Just 20)
  callData <- requiredHex fields "callData" Nothing
  pair <- case (KM.lookup "factory" fields,KM.lookup "factoryData" fields) of
    (Nothing,Nothing) -> pure (Nothing,Nothing)
    (Just _,Just _) -> (,) <$> (Just <$> requiredHex fields "factory" (Just 20)) <*> (Just <$> requiredHex fields "factoryData" Nothing)
    _ -> Left $ Legacy.invalidParams "Factory fields must be paired"
  let intent = PreparationIntent identifier sender callData (fst pair) (snd pair) resumeOnly
  -- Reuse the strict reviewed account ABI / action parser, including index and
  -- factory checks. No caller-controlled signature reaches this path.
  request <- internalRequest "eth_estimateUserOperationGas" [Object $ unsignedSkeleton intent, String entryPoint]
  _ <- Legacy.validateMethodParams request
  pure intent
 where
  lower (String value) = String $ T.toLower value
  lower value = value
parsePreparationIntent _ = Left $ Legacy.invalidParams "Preparation requires one versioned intent object"

requiredHex :: KM.KeyMap Value -> Key.Key -> Maybe Int -> Either Legacy.ProxyFailure Text
requiredHex fields name bytes = case KM.lookup name fields of
  Just (String value)
    | T.isPrefixOf "0x" value, even (T.length value), T.all isHexDigit (T.drop 2 value)
    , maybe (T.length value <= 524288) (\n -> T.length value == 2 + 2*n) bytes -> Right $ T.toLower value
  _ -> Left $ Legacy.invalidParams "Preparation contains invalid hex data"

entryPoint :: Text
entryPoint = "0x4337084d9e255ff0702461cf8895ce9e3b5ff108"

unsignedSkeleton :: PreparationIntent -> KM.KeyMap Value
unsignedSkeleton intent = KM.fromList $
  [ ("sender",String $ piSender intent), ("callData",String $ piCallData intent)
  , ("nonce",String "0x0"), ("signature",String Legacy.dummySignature)
  ] ++ [(key,String "0x1") | key <- ["callGasLimit","verificationGasLimit","preVerificationGas","maxFeePerGas","maxPriorityFeePerGas"]]
    ++ maybe [] (\value -> [("factory",String value)]) (piFactory intent)
    ++ maybe [] (\value -> [("factoryData",String value)]) (piFactoryData intent)

intentHash :: PreparationIntent -> Text
intentHash intent = "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 $ "PletherPreparation/v1" <> LBS.toStrict (encode $ unsignedSkeleton intent))

matchesIntent :: PreparationIntent -> KM.KeyMap Value -> Bool
matchesIntent intent operation = all (\key -> KM.lookup key operation == KM.lookup key (unsignedSkeleton intent))
  ["sender", "callData", "factory", "factoryData"]

internalRequest :: Text -> [Value] -> Either Legacy.ProxyFailure Legacy.RpcRequest
internalRequest method params = Legacy.parseRpcRequest $ object
  -- Alto 1.2.7 accepts numeric request IDs; string IDs fail its request schema.
  -- Each internal request has its own HTTP response and retains strict ID checks.
  ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= method, "params" .= params]

-- Locators contain no credentials or transaction payload. Exactly one ID is required.
data PreparationLocator = PreparationLocator
  { plSender :: Text, plIdentifier :: Maybe Text, plHash :: Maybe Text
  } deriving stock (Eq, Show)

parsePreparationLocator :: [Value] -> Either Legacy.ProxyFailure PreparationLocator
parsePreparationLocator [Object fields] = do
  unless (all (`elem` ["version","chainId","sender","preparationId","userOperationHash"]) $ KM.keys fields) $
    Left $ Legacy.invalidParams "Preparation status contains unsupported fields"
  unless (KM.lookup "version" fields == Just (Number 1) && KM.lookup "chainId" fields == Just (String "0x66eee")) $
    Left $ Legacy.invalidParams "Unsupported preparation status locator"
  sender <- requiredHex fields "sender" (Just 20)
  case (KM.member "preparationId" fields, KM.member "userOperationHash" fields) of
    (True, False) -> PreparationLocator sender . Just <$> requiredHex fields "preparationId" (Just 32) <*> pure Nothing
    (False, True) -> PreparationLocator sender Nothing . Just <$> requiredHex fields "userOperationHash" (Just 32)
    _ -> Left $ Legacy.invalidParams "Exactly one preparation ID or operation hash is required"
parsePreparationLocator _ = Left $ Legacy.invalidParams "Preparation status requires one versioned locator"

-- Recoverability is guidance only: the preparation endpoint revalidates the
-- immutable batch. An expiry timestamp never releases a reservation here.
preparationStatusResponse :: Integer -> Maybe Integer -> Bool -> KM.KeyMap Value -> Value
preparationStatusResponse now safeTimestamp requestedAssistance fields = Object $ KM.union (KM.fromList
  [("version",Number 1),("serverTime",String $ T.pack $ show now),
   ("safeBlockTimestamp",maybe Null (String . T.pack . show) safeTimestamp),
   ("phase",String phase),("reason",String reason),("recoverable",Bool resume),
   ("freshReviewAllowed",Bool $ not blocked && (resolved || (not observed && state `elem` map (Just . String) ["signed","preparing"])))]) fields
 where
  state = KM.lookup "authorizationState" fields
  expiry = KM.lookup "validUntil" fields >>= \case
    String value -> readMaybe (T.unpack value)
    _ -> Nothing
  expired = maybe False (<= now) expiry
  settled = state == Just (String "settled")
  resolved = state `elem` map (Just . String) ["expired","cancelled"] || settled
  assisted = requestedAssistance || KM.lookup "assisted" fields == Just (Bool True)
  blocked = assisted && KM.lookup "assistanceBlocked" fields == Just (Bool True)
  observed = case KM.lookup "transactionHash" fields of Just (String _) -> True; _ -> False
  resume = not observed && not (blocked && state == Just (String "preparing"))
    && state `elem` map (Just . String) ["signed","preparing"] && not expired
    && KM.lookup "preparationAvailable" fields == Just (Bool True)
  phase :: Text
  phase | settled = "settled"
        | Just (String _) <- KM.lookup "transactionHash" fields = "included"
        | state == Just (String "submitted") = "submitted"
        | expired && not resolved = "expiry-awaiting-reconciliation"
        | resolved = "resolved"
        | otherwise = "prepared"
  reason :: Text
  reason | settled && assisted && KM.lookup "executionSuccess" fields == Just (Bool True) && KM.lookup "assistanceVerified" fields == Just (Bool True) = "INTENT_ALREADY_COMMITTED"
         | blocked && expired && not resolved = "SAFE_EXPIRY_WAIT"
         | blocked && not resume = "ASSISTANCE_RESERVATION_PENDING"
         | resume = "RESUMABLE"
         | otherwise = "PREPARATION_UNUSABLE"
