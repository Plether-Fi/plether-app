module Plether.Perps.Release
  ( perpsV2ManifestVersion
  , perpsV2CalldataPolicy
  , perpsV2DeploymentBlock
  , perpsV2VolumeHistoryStartTimestamp
  , perpsV2OrderRouter
  , perpsV2OrderLifecycleBook
  , perpsV2PolicyEvaluator
  , perpsV2PositionProtectionBook
  , perpsV2PublicLens
  , validatePerpsV2ReleaseConfig
  , verifyPerpsV2ReleaseBindings
  ) where

import qualified Plether.Perps.Manifest as Manifest
import Control.Monad (foldM, unless)
import Data.Aeson (Value (..), toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi (decodeAddress, keccak256, selector)
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , ethBlockNumber
  , ethCallAtBlock
  , rpcCall
  )
import Plether.Utils.Hex (intToHex)

perpsV2ManifestVersion :: Text
perpsV2ManifestVersion = Manifest.releaseAaManifestVersion

perpsV2CalldataPolicy :: Text
perpsV2CalldataPolicy = "bounded-v2"

perpsV2DeploymentBlock :: Integer
perpsV2DeploymentBlock = Manifest.releaseDeploymentBlock

-- | First whole minute after the pinned V2 deployment block. The history
-- indexer starts at the deployment block, so a certified cursor proves the
-- intervening zero-volume minutes even when the release activation record was
-- written later.
perpsV2VolumeHistoryStartTimestamp :: Integer
perpsV2VolumeHistoryStartTimestamp = Manifest.releaseVolumeHistoryStartTimestamp

perpsV2OrderRouter :: Text
perpsV2OrderRouter = Manifest.orderRouterAddress

perpsV2OrderLifecycleBook :: Text
perpsV2OrderLifecycleBook = Manifest.orderLifecycleBookAddress

perpsV2PolicyEvaluator :: Text
perpsV2PolicyEvaluator = Manifest.cfdOrderPolicyEvaluatorAddress

perpsV2PositionProtectionBook :: Text
perpsV2PositionProtectionBook = Manifest.positionProtectionBookAddress

perpsV2PublicLens :: Text
perpsV2PublicLens = Manifest.perpsPublicLensAddress

perpsV2Engine :: Text
perpsV2Engine = Manifest.cfdEngineAddress

perpsV2Clearinghouse :: Text
perpsV2Clearinghouse = Manifest.marginClearinghouseAddress

perpsV2HousePool :: Text
perpsV2HousePool = Manifest.housePoolAddress

validatePerpsV2ReleaseConfig
  :: Integer
  -> Text
  -> Maybe Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Either Text ()
validatePerpsV2ReleaseConfig chainId router lifecycleBook engine clearinghouse housePool startBlock = do
  expectNumber "PERPS_CHAIN_ID" chainId Manifest.releaseChainId
  expectAddress "PERPS_ORDER_ROUTER" router perpsV2OrderRouter
  case lifecycleBook of
    Nothing -> Left "PERPS_ORDER_LIFECYCLE_BOOK is required for bounded V2 sponsorship"
    Just address -> expectAddress "PERPS_ORDER_LIFECYCLE_BOOK" address perpsV2OrderLifecycleBook
  expectAddress "PERPS_CFD_ENGINE" engine perpsV2Engine
  expectAddress "PERPS_MARGIN_CLEARINGHOUSE" clearinghouse perpsV2Clearinghouse
  expectAddress "PERPS_HOUSE_POOL" housePool perpsV2HousePool
  expectNumber "PERPS_INDEXER_START_BLOCK" startBlock perpsV2DeploymentBlock
 where
  expectAddress label actual expected =
    unless (T.toLower actual == T.toLower expected) $
      Left $ label <> " must match the pinned bounded V2 release " <> expected
  expectNumber label actual expected =
    unless (actual == expected) $
      Left $ label <> " must match the pinned bounded V2 release " <> T.pack (show expected)

verifyPerpsV2ReleaseBindings
  :: EthClient
  -> Integer
  -> Text
  -> Maybe Text
  -> Text
  -> Text
  -> Text
  -> Integer
  -> IO (Either Text Integer)
verifyPerpsV2ReleaseBindings client chainId router maybeLifecycle engine clearinghouse housePool startBlock =
  case validatePerpsV2ReleaseConfig chainId router maybeLifecycle engine clearinghouse housePool startBlock of
    Left failure -> pure $ Left failure
    Right () -> do
      latest <- ethBlockNumber client
      case latest of
        Left failure -> pure $ Left $ "Could not resolve the Sepolia binding-check block: " <> T.pack (show failure)
        Right blockNumber -> do
          let lifecycle = perpsV2OrderLifecycleBook
              addressChecks =
                [ ("Router Engine", router, "engine()", engine)
                , ("Router LifecycleBook", router, "lifecycleBook()", lifecycle)
                , ("Router policy evaluator", router, "policyEvaluator()", perpsV2PolicyEvaluator)
                , ("Router position-protection Book", router, "positionProtectionBook()", perpsV2PositionProtectionBook)
                , ("Lifecycle Router", lifecycle, "ROUTER()", router)
                , ("Lifecycle Engine", lifecycle, "ENGINE()", engine)
                , ("Lifecycle Clearinghouse", lifecycle, "CLEARINGHOUSE()", clearinghouse)
                , ("Lifecycle HousePool", lifecycle, "HOUSE_POOL()", housePool)
                , ("Engine Clearinghouse", engine, "clearinghouse()", clearinghouse)
                , ("Engine Pool", engine, "pool()", housePool)
                , ("Public lens Engine", perpsV2PublicLens, "ENGINE()", engine)
                , ("Public lens Router", perpsV2PublicLens, "ORDER_ROUTER()", router)
                , ("Public lens HousePool", perpsV2PublicLens, "HOUSE_POOL()", housePool)
                , ("Position-protection Router", perpsV2PositionProtectionBook, "ROUTER()", router)
                ]
          bindings <- foldM (verifyAddressAt client blockNumber) (Right ()) addressChecks
          case bindings of
            Left failure -> pure $ Left failure
            Right () -> do
              hashes <- foldM (verifyRuntimeHashAt client blockNumber) (Right ()) runtimeHashes
              pure $ blockNumber <$ hashes

verifyAddressAt
  :: EthClient
  -> Integer
  -> Either Text ()
  -> (Text, Text, Text, Text)
  -> IO (Either Text ())
verifyAddressAt _ _ failure@(Left _) _ = pure failure
verifyAddressAt client blockNumber (Right ()) (label, target, signature, expected) = do
  result <- ethCallAtBlock client (CallParams target $ selector signature) blockNumber
  pure $ case result of
    Left failure -> Left $ label <> " binding read failed: " <> T.pack (show failure)
    Right bytes
      | BS.length bytes /= 32 -> Left $ label <> " binding response was not one ABI word"
      | T.toLower (decodeAddress bytes) == T.toLower expected -> Right ()
      | otherwise -> Left $ label <> " binding mismatch: expected " <> expected <> ", received " <> decodeAddress bytes

runtimeHashes :: [(Text, Text, Text)]
runtimeHashes =
  [ ("OrderRouter", perpsV2OrderRouter, Manifest.orderRouterCodeHash)
  , ("OrderLifecycleBook", perpsV2OrderLifecycleBook, Manifest.orderLifecycleBookCodeHash)
  , ("PolicyEvaluator", perpsV2PolicyEvaluator, Manifest.cfdOrderPolicyEvaluatorCodeHash)
  , ("PositionProtectionBook", perpsV2PositionProtectionBook, Manifest.positionProtectionBookCodeHash)
  , ("CfdEngine", perpsV2Engine, Manifest.cfdEngineCodeHash)
  , ("MarginClearinghouse", perpsV2Clearinghouse, Manifest.marginClearinghouseCodeHash)
  , ("HousePool", perpsV2HousePool, Manifest.housePoolCodeHash)
  , ("PerpsPublicLens", perpsV2PublicLens, Manifest.perpsPublicLensCodeHash)
  ]

verifyRuntimeHashAt
  :: EthClient
  -> Integer
  -> Either Text ()
  -> (Text, Text, Text)
  -> IO (Either Text ())
verifyRuntimeHashAt _ _ failure@(Left _) _ = pure failure
verifyRuntimeHashAt client blockNumber (Right ()) (label, address, expected) = do
  result <- rpcCall client "eth_getCode" $ toJSON [String address, String $ "0x" <> intToHex blockNumber]
  pure $ case result of
    Left failure -> Left $ label <> " runtime-code read failed: " <> T.pack (show failure)
    Right (String encoded)
      | not ("0x" `T.isPrefixOf` encoded) || odd (T.length encoded - 2) ->
          Left $ label <> " runtime code was not canonical hex"
      | otherwise ->
          case B16.decode $ TE.encodeUtf8 $ T.drop 2 encoded of
            Left _ -> Left $ label <> " runtime code was not canonical hex"
            Right runtimeCode
              | BS.null runtimeCode -> Left $ label <> " runtime code was empty"
              | otherwise ->
                  let actual = "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 runtimeCode)
                   in if T.toLower actual == T.toLower expected
                        then Right ()
                        else Left $ label <> " runtime-code hash mismatch: expected " <> expected <> ", received " <> actual
    Right _ -> Left $ label <> " runtime-code response was not a hex string"
