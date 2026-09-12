module Plether.AA.Preparation
  ( PreparationIntent (..), parsePreparationIntent, unsignedSkeleton, intentHash, matchesIntent, internalRequest
  , gasPolicyVersion, executionGasWithHeadroom ) where

import Control.Monad (unless)
import Data.Aeson (Value (..), object, encode, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Plether.AA.Pimlico as Legacy
import Plether.Ethereum.Abi (keccak256)

data PreparationIntent = PreparationIntent
  { piIdentifier :: Text, piSender :: Text, piCallData :: Text
  , piFactory :: Maybe Text, piFactoryData :: Maybe Text
  } deriving stock (Eq, Show)

-- Applied ONLY to a fresh Alto estimate, before immutable preparation storage.
-- Changing this identifier requires a new reviewed preparation, never a rewrite
-- of a persisted/signed operation or its reservation.
gasPolicyVersion :: Text
gasPolicyVersion = "execution-headroom-v1-150pct-min100000"

executionGasWithHeadroom :: Integer -> Either Text Integer
executionGasWithHeadroom estimated = do
  unless (estimated > 0 && estimated <= 2_000_000) $
    Left "Invalid execution gas estimate"
  let padded = max ((estimated * 3 + 1) `div` 2) (estimated + 100_000)
  unless (padded <= 2_000_000) $
    Left "Execution gas including headroom exceeds the reviewed sponsorship bounds"
  pure padded

parsePreparationIntent :: [Value] -> Either Legacy.ProxyFailure PreparationIntent
parsePreparationIntent [Object fields] = do
  unless (all (`elem` ["version","preparationId","chainId","entryPoint","sender","callData","factory","factoryData"]) $ KM.keys fields) $
    Left $ Legacy.invalidParams "Preparation contains unsupported fields"
  unless (KM.lookup "version" fields == Just (Number 1)
    && KM.lookup "chainId" fields == Just (String "0x66eee")
    && fmap lower (KM.lookup "entryPoint" fields) == Just (String entryPoint)) $
    Left $ Legacy.invalidParams "Unsupported preparation version, chain or EntryPoint"
  identifier <- requiredHex fields "preparationId" (Just 32)
  sender <- requiredHex fields "sender" (Just 20)
  callData <- requiredHex fields "callData" Nothing
  pair <- case (KM.lookup "factory" fields,KM.lookup "factoryData" fields) of
    (Nothing,Nothing) -> pure (Nothing,Nothing)
    (Just _,Just _) -> (,) <$> (Just <$> requiredHex fields "factory" (Just 20)) <*> (Just <$> requiredHex fields "factoryData" Nothing)
    _ -> Left $ Legacy.invalidParams "Factory fields must be paired"
  let intent = PreparationIntent identifier sender callData (fst pair) (snd pair)
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
