module Plether.Perps.Release
  ( perpsV2ManifestVersion
  , perpsV2CalldataPolicy
  , perpsV2DeploymentBlock
  , perpsV2OrderRouter
  , perpsV2OrderLifecycleBook
  , perpsV2PolicyEvaluator
  , perpsV2PositionProtectionBook
  , perpsV2PublicLens
  , validatePerpsV2ReleaseConfig
  , verifyPerpsV2ReleaseBindings
  ) where

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
perpsV2ManifestVersion = "perps-aa-arbitrum-sepolia-20260830-v2"

perpsV2CalldataPolicy :: Text
perpsV2CalldataPolicy = "bounded-v2"

perpsV2DeploymentBlock :: Integer
perpsV2DeploymentBlock = 302257125

perpsV2OrderRouter :: Text
perpsV2OrderRouter = "0x97A901dE2B267c307E264FD5F71403F8072F73e7"

perpsV2OrderLifecycleBook :: Text
perpsV2OrderLifecycleBook = "0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E"

perpsV2PolicyEvaluator :: Text
perpsV2PolicyEvaluator = "0xaa4703B190684b5A57b8a9aA432fA043B169D171"

perpsV2PositionProtectionBook :: Text
perpsV2PositionProtectionBook = "0xC009E2159146188b272420cF273B0fc12e5Fdfc8"

perpsV2PublicLens :: Text
perpsV2PublicLens = "0xC41e92F541cCF19FA203a96CecF3Ae4D2Ed7F60A"

perpsV2Engine :: Text
perpsV2Engine = "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D"

perpsV2Clearinghouse :: Text
perpsV2Clearinghouse = "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211"

perpsV2HousePool :: Text
perpsV2HousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"

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
  expectNumber "PERPS_CHAIN_ID" chainId 421614
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
  [ ("OrderRouter", perpsV2OrderRouter, "0x74f3676e93f5175dddec2298ad0fdc67bdc7436c8ecabc59ed980f8ee8a7881a")
  , ("OrderLifecycleBook", perpsV2OrderLifecycleBook, "0xaf0f48b06282386e245935921b72f8f4a2fe8e43afb89bb1a7d3eba3962e9517")
  , ("PolicyEvaluator", perpsV2PolicyEvaluator, "0xbf5d49312d5ca849e2719bef0dda2a45c552fb9b5af3e626537ff72adaa85882")
  , ("PositionProtectionBook", perpsV2PositionProtectionBook, "0xb24932b1130c74c32279aef54032368fdf61a16ff4fedbc8c0e3d6e6430fc42c")
  , ("CfdEngine", perpsV2Engine, "0xf61a42cb75e6b83ccbbb1c7046f41d75c9d793de4105a6fe259eb8174c9b8b42")
  , ("MarginClearinghouse", perpsV2Clearinghouse, "0xe761ef9f2249d04f20264dfb1f04895f2864aeaa407782e74d7b555f8a39f7e9")
  , ("HousePool", perpsV2HousePool, "0x730fe4c35663b034934191001a219c487b53cad00a5ad706b9767103a61f18c1")
  , ("PerpsPublicLens", perpsV2PublicLens, "0x8503b7db5845a8aff3dc7cb82c9418351136b1e7aa10ecbb374ea5df78c014b3")
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
