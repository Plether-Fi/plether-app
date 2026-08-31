module Plether.Ethereum.Contracts.SettlementMonitor
  ( ExecutionPath (..)
  , SettlementStatus (..)
  , SettlementObservation (..)
  , SettlementDeployment (..)
  , SettlementCodeHashes (..)
  , reviewedV120SettlementCodeHashes
  , supportedConfigSchemaVersion
  , supportedObservationSchemaVersion
  , verifyBindings
  , readSettlementDeployment
  , validateSettlementDeployment
  , verifySettlementDeployment
  , getCurrentEpoch
  , getCurrentEpochAtBlock
  , getSettlementStatus
  , getSettlementObservationAtBlock
  , decodeSettlementStatus
  , decodeSettlementObservation
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi
  ( decodeAddress
  , decodeUint256
  , encodeCall
  , encodeUint256
  , keccak256
  )
import Plether.Ethereum.Client
  ( CallParams (..)
  , EthClient
  , RpcError (..)
  , ethCall
  , ethCallAtBlock
  )
import Plether.Ethereum.Rpc (ethGetCode)

data ExecutionPath
  = UnknownPath
  | NoMaturedWork
  | CachedMark
  | AtomicOracleRefresh
  deriving stock (Show, Eq)

data SettlementStatus = SettlementStatus
  { ssObservedBlock :: Integer
  , ssCurrentEpoch :: Integer
  , ssSettlementCutoffEpoch :: Integer
  , ssMinimumAtomicPublishTime :: Integer
  , ssSeniorMaturedDepositHeadEpoch :: Maybe Integer
  , ssSeniorMaturedDepositHeadAssets :: Integer
  , ssSeniorMaturedRedeemHeadEpoch :: Maybe Integer
  , ssSeniorMaturedRedeemHeadShares :: Integer
  , ssJuniorMaturedDepositHeadEpoch :: Maybe Integer
  , ssJuniorMaturedDepositHeadAssets :: Integer
  , ssJuniorMaturedRedeemHeadEpoch :: Maybe Integer
  , ssJuniorMaturedRedeemHeadShares :: Integer
  , ssOldestMaturedHead :: Maybe Integer
  , ssRequiredExecutionPath :: ExecutionPath
  , ssCachedMarkPrice :: Integer
  , ssCachedMarkTime :: Integer
  , ssOperationalBlockerMask :: Integer
  , ssWarningMask :: Integer
  , ssExecutionPathDependencyMask :: Integer
  , ssDependencyFailureMask :: Integer
  , ssHasMaturedWork :: Bool
  , ssLpEpochSettlementPaused :: Bool
  }
  deriving stock (Show, Eq)

-- | Exact contracts that participate in an LP settlement deployment. The
-- schema version is deliberately part of the expected binding: the monitor's
-- large, static return tuples cannot be decoded safely across schema changes.
data SettlementDeployment = SettlementDeployment
  { sdConfigSchemaVersion :: Integer
  , sdMonitor :: Text
  , sdRouter :: Text
  , sdEngine :: Text
  , sdHousePool :: Text
  , sdSeniorVault :: Text
  , sdJuniorVault :: Text
  , sdPletherOracle :: Text
  }
  deriving stock (Show, Eq)

data SettlementCodeHashes = SettlementCodeHashes
  { schMonitor :: Text
  , schRouter :: Text
  , schEngine :: Text
  , schHousePool :: Text
  , schSeniorVault :: Text
  , schJuniorVault :: Text
  , schPletherOracle :: Text
  }
  deriving stock (Show, Eq)

-- Keccak-256 hashes of the runtime bytecode at the reviewed Arbitrum Sepolia
-- v1.2.0 release addresses. These are deliberately compiled into the keeper:
-- changing any execution-critical implementation requires a reviewed release.
reviewedV120SettlementCodeHashes :: SettlementCodeHashes
reviewedV120SettlementCodeHashes =
  SettlementCodeHashes
    { schMonitor = "0x625558c5479800ddf19d07fd537c53659f4f731d6327fd6dd10ca8cee4759c0c"
    , schRouter = "0x74f3676e93f5175dddec2298ad0fdc67bdc7436c8ecabc59ed980f8ee8a7881a"
    , schEngine = "0xf61a42cb75e6b83ccbbb1c7046f41d75c9d793de4105a6fe259eb8174c9b8b42"
    , schHousePool = "0x730fe4c35663b034934191001a219c487b53cad00a5ad706b9767103a61f18c1"
    , schSeniorVault = "0xe370e3cb4330e38ee9836598bba279cde16d51a5c68375c2bc6afe18a5ba4cea"
    , schJuniorVault = "0x36a4714a180c98221b06f0cb6edf85a525fb36cfa3d511183507140e341920ba"
    , schPletherOracle = "0xe25325224a61a901d6bb7d9f0000c054252d350fdb3ca103a6e3b60c59b64850"
    }

supportedConfigSchemaVersion :: Integer
supportedConfigSchemaVersion = 4

-- The deployed v1.2.0 facade writes CONFIG_SCHEMA_VERSION into observation
-- word zero. Keep the observation gate named separately so a future facade
-- can version the two surfaces independently without weakening validation.
supportedObservationSchemaVersion :: Integer
supportedObservationSchemaVersion = supportedConfigSchemaVersion

data SettlementObservation = SettlementObservation
  { soSchemaVersion :: Integer
  , soStatus :: SettlementStatus
  , soHealthState :: Integer
  , soCriticalFaultMask :: Integer
  , soHealthDependencyFailureMask :: Integer
  , soObservationDigest :: Text
  , soObservationComplete :: Bool
  }
  deriving stock (Show, Eq)

verifyBindings
  :: EthClient
  -> Text
  -> Text
  -> Text
  -> IO (Either Text ())
verifyBindings client monitor expectedRouter expectedHousePool = do
  routerResult <- readAddress client monitor "ROUTER()"
  poolResult <- readAddress client monitor "HOUSE_POOL()"
  pure $ do
    router <- firstRpc routerResult
    pool <- firstRpc poolResult
    if normalize router /= normalize expectedRouter
      then Left $ "Settlement Monitor ROUTER binding mismatch: expected " <> expectedRouter <> ", observed " <> router
      else
        if normalize pool /= normalize expectedHousePool
          then Left $ "Settlement Monitor HOUSE_POOL binding mismatch: expected " <> expectedHousePool <> ", observed " <> pool
          else Right ()

-- | Read the deployment graph directly from the monitor facade. This does not
-- trust the monitor's own health verdict; 'verifySettlementDeployment' also
-- checks code presence and the reverse bindings on every execution-critical
-- contract.
readSettlementDeployment :: EthClient -> Text -> IO (Either Text SettlementDeployment)
readSettlementDeployment client monitor = runExceptT $ do
  schema <- rpcRead $ readUint client monitor "CONFIG_SCHEMA_VERSION()"
  router <- rpcRead $ readAddress client monitor "ROUTER()"
  engine <- rpcRead $ readAddress client monitor "ENGINE()"
  housePool <- rpcRead $ readAddress client monitor "HOUSE_POOL()"
  seniorVault <- rpcRead $ readAddress client monitor "SENIOR_VAULT()"
  juniorVault <- rpcRead $ readAddress client monitor "JUNIOR_VAULT()"
  pletherOracle <- rpcRead $ readAddress client router "pletherOracle()"
  pure
    SettlementDeployment
      { sdConfigSchemaVersion = schema
      , sdMonitor = monitor
      , sdRouter = router
      , sdEngine = engine
      , sdHousePool = housePool
      , sdSeniorVault = seniorVault
      , sdJuniorVault = juniorVault
      , sdPletherOracle = pletherOracle
      }

-- | Compare the configured deployment with one observed from the monitor.
-- Address comparisons are case-insensitive but otherwise exact.
validateSettlementDeployment
  :: SettlementDeployment
  -> SettlementDeployment
  -> Either Text ()
validateSettlementDeployment expected observed = do
  if sdConfigSchemaVersion observed /= sdConfigSchemaVersion expected
    then
      Left $
        "Settlement Monitor CONFIG_SCHEMA_VERSION mismatch: expected "
          <> tshow (sdConfigSchemaVersion expected)
          <> ", observed "
          <> tshow (sdConfigSchemaVersion observed)
    else Right ()
  forM_
    [ ("monitor", sdMonitor expected, sdMonitor observed)
    , ("router", sdRouter expected, sdRouter observed)
    , ("engine", sdEngine expected, sdEngine observed)
    , ("HousePool", sdHousePool expected, sdHousePool observed)
    , ("Senior vault", sdSeniorVault expected, sdSeniorVault observed)
    , ("Junior vault", sdJuniorVault expected, sdJuniorVault observed)
    , ("Plether oracle", sdPletherOracle expected, sdPletherOracle observed)
    ]
    $ \(label, expectedAddress, observedAddress) ->
      requireAddressMatch label expectedAddress observedAddress

-- | Fail-closed startup verification for the settlement execution graph.
-- Besides checking the facade, this validates bytecode and the reverse
-- Router/Engine/HousePool/vault relationships used by both settlement paths.
verifySettlementDeployment
  :: EthClient
  -> SettlementDeployment
  -> SettlementCodeHashes
  -> IO (Either Text ())
verifySettlementDeployment client expected expectedCodeHashes = runExceptT $ do
  observed <- ExceptT $ readSettlementDeployment client (sdMonitor expected)
  ExceptT $ pure $ validateSettlementDeployment expected observed
  forM_
    [ ("Settlement Monitor", sdMonitor expected, schMonitor expectedCodeHashes)
    , ("Order Router", sdRouter expected, schRouter expectedCodeHashes)
    , ("CFD Engine", sdEngine expected, schEngine expectedCodeHashes)
    , ("HousePool", sdHousePool expected, schHousePool expectedCodeHashes)
    , ("Senior vault", sdSeniorVault expected, schSeniorVault expectedCodeHashes)
    , ("Junior vault", sdJuniorVault expected, schJuniorVault expectedCodeHashes)
    , ("Plether oracle", sdPletherOracle expected, schPletherOracle expectedCodeHashes)
    ]
    $ \(label, address, expectedHash) ->
      requireContractCodeHash client label address expectedHash

  requireAddressBinding client "Order Router engine()" (sdRouter expected) "engine()" (sdEngine expected)
  requireAddressBinding
    client
    "Order Router pletherOracle()"
    (sdRouter expected)
    "pletherOracle()"
    (sdPletherOracle expected)
  requireAddressBinding client "CFD Engine orderRouter()" (sdEngine expected) "orderRouter()" (sdRouter expected)
  requireAddressBinding client "HousePool ENGINE()" (sdHousePool expected) "ENGINE()" (sdEngine expected)
  requireAddressBinding
    client
    "HousePool seniorVault()"
    (sdHousePool expected)
    "seniorVault()"
    (sdSeniorVault expected)
  requireAddressBinding
    client
    "HousePool juniorVault()"
    (sdHousePool expected)
    "juniorVault()"
    (sdJuniorVault expected)
  requireAddressBinding client "Senior vault POOL()" (sdSeniorVault expected) "POOL()" (sdHousePool expected)
  requireAddressBinding client "Junior vault POOL()" (sdJuniorVault expected) "POOL()" (sdHousePool expected)
  requireBoolBinding client "Senior vault IS_SENIOR()" (sdSeniorVault expected) "IS_SENIOR()" True
  requireBoolBinding client "Junior vault IS_SENIOR()" (sdJuniorVault expected) "IS_SENIOR()" False

getCurrentEpoch :: EthClient -> Text -> IO (Either RpcError Integer)
getCurrentEpoch client housePool = do
  result <- ethCall client $ CallParams housePool $ encodeCall "currentLpEpoch()" []
  pure $ decodeSingleWord "currentLpEpoch()" result

getCurrentEpochAtBlock :: EthClient -> Text -> Integer -> IO (Either RpcError Integer)
getCurrentEpochAtBlock client housePool blockNumber = do
  result <-
    ethCallAtBlock
      client
      (CallParams housePool $ encodeCall "currentLpEpoch()" [])
      blockNumber
  pure $ decodeSingleWord "currentLpEpoch()" result

getSettlementStatus
  :: EthClient
  -> Text
  -> Integer
  -> IO (Either RpcError SettlementStatus)
getSettlementStatus client monitor epoch = do
  result <-
    ethCall client $
      CallParams monitor $
        encodeCall "getSettlementStatus(uint256)" [encodeUint256 epoch]
  pure $ result >>= decodeSettlementStatus

getSettlementObservationAtBlock
  :: EthClient
  -> Text
  -> Integer
  -> Integer
  -> IO (Either RpcError SettlementObservation)
getSettlementObservationAtBlock client monitor epoch blockNumber = do
  result <-
    ethCallAtBlock
      client
      ( CallParams monitor $
          encodeCall "getSettlementObservation(uint256)" [encodeUint256 epoch]
      )
      blockNumber
  pure $ result >>= decodeSettlementObservation

decodeSettlementStatus :: ByteString -> Either RpcError SettlementStatus
decodeSettlementStatus bytes
  | BS.length bytes /= settlementStatusWordCount * 32 =
      Left $ RpcJsonError "getSettlementStatus(uint256) did not return exactly 109 ABI words"
  | otherwise = decodeStatusAt 0 bytes

decodeSettlementObservation :: ByteString -> Either RpcError SettlementObservation
decodeSettlementObservation bytes
  | BS.length bytes /= observationWordCount * 32 =
      Left $ RpcJsonError "getSettlementObservation(uint256) did not return exactly 194 ABI words"
  | otherwise = do
      status <- decodeStatusAt 1 bytes
      observationComplete <-
        decodeCanonicalBool
          "getSettlementObservation(uint256).observationComplete"
          (wordBytesAt 193 bytes)
      pure
        SettlementObservation
          { soSchemaVersion = wordAt 0 bytes
          , soStatus = status
          , soHealthState = wordAt 159 bytes
          , soCriticalFaultMask = wordAt 160 bytes
          , soHealthDependencyFailureMask = wordAt 161 bytes
          , soObservationDigest = bytes32Hex $ wordBytesAt 191 bytes
          , soObservationComplete = observationComplete
          }

decodeStatusAt :: Int -> ByteString -> Either RpcError SettlementStatus
decodeStatusAt base bytes
  | BS.length bytes < (base + settlementStatusWordCount) * 32 =
      Left $ RpcJsonError "getSettlementStatus(uint256) returned fewer than 109 ABI words"
  | otherwise = do
      let seniorDepositAssets = wordAt (base + 24) bytes
          seniorRedeemShares = wordAt (base + 26) bytes
          juniorDepositAssets = wordAt (base + 59) bytes
          juniorRedeemShares = wordAt (base + 61) bytes
          seniorDepositEpoch = presentHead seniorDepositAssets $ wordAt (base + 23) bytes
          seniorRedeemEpoch = presentHead seniorRedeemShares $ wordAt (base + 25) bytes
          juniorDepositEpoch = presentHead juniorDepositAssets $ wordAt (base + 58) bytes
          juniorRedeemEpoch = presentHead juniorRedeemShares $ wordAt (base + 60) bytes
          oldestHead = minimumMaybe $ catMaybes [seniorDepositEpoch, seniorRedeemEpoch, juniorDepositEpoch, juniorRedeemEpoch]
      hasMaturedWork <-
        decodeCanonicalBool
          "getSettlementStatus(uint256).hasMaturedWork"
          (wordBytesAt (base + 99) bytes)
      lpEpochSettlementPaused <-
        decodeCanonicalBool
          "getSettlementStatus(uint256).lpEpochSettlementPaused"
          (wordBytesAt (base + 108) bytes)
      pure
        SettlementStatus
          { ssObservedBlock = wordAt (base + 1) bytes
          , ssCurrentEpoch = wordAt (base + 4) bytes
          , ssSettlementCutoffEpoch = wordAt (base + 5) bytes
          , ssMinimumAtomicPublishTime = wordAt (base + 11) bytes
          , ssSeniorMaturedDepositHeadEpoch = seniorDepositEpoch
          , ssSeniorMaturedDepositHeadAssets = seniorDepositAssets
          , ssSeniorMaturedRedeemHeadEpoch = seniorRedeemEpoch
          , ssSeniorMaturedRedeemHeadShares = seniorRedeemShares
          , ssJuniorMaturedDepositHeadEpoch = juniorDepositEpoch
          , ssJuniorMaturedDepositHeadAssets = juniorDepositAssets
          , ssJuniorMaturedRedeemHeadEpoch = juniorRedeemEpoch
          , ssJuniorMaturedRedeemHeadShares = juniorRedeemShares
          , ssOldestMaturedHead = oldestHead
          , ssRequiredExecutionPath = decodeExecutionPath $ wordAt (base + 86) bytes
          , ssCachedMarkPrice = wordAt (base + 87) bytes
          , ssCachedMarkTime = wordAt (base + 88) bytes
          , ssOperationalBlockerMask = wordAt (base + 93) bytes
          , ssWarningMask = wordAt (base + 94) bytes
          , ssExecutionPathDependencyMask = wordAt (base + 97) bytes
          , ssDependencyFailureMask = wordAt (base + 98) bytes
          , ssHasMaturedWork = hasMaturedWork
          , ssLpEpochSettlementPaused = lpEpochSettlementPaused
          }

decodeCanonicalBool :: Text -> ByteString -> Either RpcError Bool
decodeCanonicalBool label bytes =
  case decodeUint256 bytes of
    0 -> Right False
    1 -> Right True
    _ -> Left $ RpcJsonError $ label <> " returned a non-canonical ABI boolean"

decodeExecutionPath :: Integer -> ExecutionPath
decodeExecutionPath = \case
  1 -> NoMaturedWork
  2 -> CachedMark
  3 -> AtomicOracleRefresh
  _ -> UnknownPath

readAddress :: EthClient -> Text -> Text -> IO (Either RpcError Text)
readAddress client target signature = do
  result <- ethCall client $ CallParams target $ encodeCall signature []
  pure $ result >>= decodeCanonicalAddressWord signature

decodeCanonicalAddressWord :: Text -> ByteString -> Either RpcError Text
decodeCanonicalAddressWord signature bytes
  | BS.length bytes /= 32 =
      Left $ RpcJsonError $ signature <> " did not return exactly one ABI word"
  | BS.any (/= 0) (BS.take 12 bytes) =
      Left $ RpcJsonError $ signature <> " returned a non-canonical ABI address"
  | otherwise = Right $ decodeAddress bytes

readUint :: EthClient -> Text -> Text -> IO (Either RpcError Integer)
readUint client target signature = do
  result <- ethCall client $ CallParams target $ encodeCall signature []
  pure $ decodeSingleWord signature result

readBool :: EthClient -> Text -> Text -> IO (Either RpcError Bool)
readBool client target signature = do
  result <- ethCall client $ CallParams target $ encodeCall signature []
  pure $ do
    bytes <- result
    if BS.length bytes /= 32
      then Left $ RpcJsonError $ signature <> " did not return exactly one ABI word"
      else case decodeUint256 bytes of
        0 -> Right False
        1 -> Right True
        _ -> Left $ RpcJsonError $ signature <> " returned a non-canonical ABI boolean"

decodeSingleWord :: Text -> Either RpcError ByteString -> Either RpcError Integer
decodeSingleWord label result = do
  bytes <- result
  if BS.length bytes /= 32
    then Left $ RpcJsonError $ label <> " did not return exactly one ABI word"
    else Right $ decodeUint256 $ wordBytesAt 0 bytes

rpcRead :: IO (Either RpcError a) -> ExceptT Text IO a
rpcRead action = ExceptT $ firstRpc <$> action

requireContractCodeHash :: EthClient -> Text -> Text -> Text -> ExceptT Text IO ()
requireContractCodeHash client label address expectedHash = do
  code <- rpcRead $ ethGetCode client address
  if BS.null code
    then ExceptT $ pure $ Left $ label <> " has no deployed bytecode at " <> address
    else do
      let observedHash = "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 code)
      if normalize observedHash == normalize expectedHash
        then pure ()
        else
          ExceptT $
            pure $
              Left $
                label
                  <> " runtime bytecode hash mismatch at "
                  <> address
                  <> ": expected "
                  <> expectedHash
                  <> ", observed "
                  <> observedHash

requireAddressBinding :: EthClient -> Text -> Text -> Text -> Text -> ExceptT Text IO ()
requireAddressBinding client label target signature expected = do
  observed <- rpcRead $ readAddress client target signature
  ExceptT $ pure $ requireAddressMatch label expected observed

requireBoolBinding :: EthClient -> Text -> Text -> Text -> Bool -> ExceptT Text IO ()
requireBoolBinding client label target signature expected = do
  observed <- rpcRead $ readBool client target signature
  if observed == expected
    then pure ()
    else
      ExceptT $
        pure $
          Left $
            label <> " mismatch: expected " <> tshow expected <> ", observed " <> tshow observed

requireAddressMatch :: Text -> Text -> Text -> Either Text ()
requireAddressMatch label expected observed
  | normalize expected == normalize observed = Right ()
  | otherwise = Left $ label <> " binding mismatch: expected " <> expected <> ", observed " <> observed

firstRpc :: Either RpcError a -> Either Text a
firstRpc = either (Left . T.pack . show) Right

normalize :: Text -> Text
normalize = T.toLower . T.strip

tshow :: Show a => a -> Text
tshow = T.pack . show

presentHead :: Integer -> Integer -> Maybe Integer
presentHead amount epoch
  | amount > 0 = Just epoch
  | otherwise = Nothing

minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe values = Just $ minimum values

bytes32Hex :: ByteString -> Text
bytes32Hex = ("0x" <>) . TE.decodeUtf8 . B16.encode

wordAt :: Int -> ByteString -> Integer
wordAt index = decodeUint256 . wordBytesAt index

wordBytesAt :: Int -> ByteString -> ByteString
wordBytesAt index = BS.take 32 . BS.drop (index * 32)

settlementStatusWordCount :: Int
settlementStatusWordCount = 109

observationWordCount :: Int
observationWordCount = 194
