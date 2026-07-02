module Plether.Config
  ( Config (..)
  , Addresses (..)
  , Deployment (..)
  , loadConfig
  , loadDeployments
  , currentAddresses
  ) where

import Data.Aeson (FromJSON (..), Value (..), eitherDecodeFileStrict, withObject, (.:))
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Config = Config
  { cfgRpcUrl :: Text
  , cfgChainId :: Integer
  , cfgPort :: Int
  , cfgCorsOrigins :: [Text]
  , cfgDeployments :: [Deployment]
  , cfgDatabaseUrl :: Maybe Text
  , cfgIndexerStartBlock :: Integer
  , cfgPythBenchmarksUrl :: Text
  , cfgPythHermesUrl :: Text
  , cfgPythApiKey :: Maybe Text
  , cfgPythBackfillDays :: Int
  , cfgPythSampleIntervalSeconds :: Integer
  , cfgPythIngestionEnabled :: Bool
  , cfgPerpsRpcUrl :: Text
  , cfgPerpsChainId :: Integer
  , cfgPerpsUsdc :: Text
  , cfgPerpsOrderRouter :: Text
  , cfgPerpsPletherOracle :: Text
  , cfgPerpsIndexerStartBlock :: Integer
  , cfgFaucetPrivateKey :: Maybe Text
  , cfgKeeperPrivateKey :: Maybe Text
  , cfgKeeperPollSeconds :: Int
  , cfgKeeperMaxBatchSize :: Int
  , cfgKeeperConfirmations :: Int
  , cfgKeeperGasBufferBps :: Integer
  , cfgKeeperFeeBufferBps :: Integer
  }
  deriving stock (Show)

data Addresses = Addresses
  { addrUsdc :: Text
  , addrDxyBear :: Text
  , addrDxyBull :: Text
  , addrSdxyBear :: Text
  , addrSdxyBull :: Text
  , addrSyntheticSplitter :: Text
  , addrCurvePool :: Text
  , addrZapRouter :: Text
  , addrLeverageRouter :: Text
  , addrBullLeverageRouter :: Text
  , addrStakingBear :: Text
  , addrStakingBull :: Text
  , addrBasketOracle :: Text
  , addrMockAdapter :: Text
  , addrMorphoOracleBear :: Text
  , addrMorphoOracleBull :: Text
  , addrStakedOracleBear :: Text
  , addrStakedOracleBull :: Text
  , addrMorpho :: Text
  , addrMorphoMarketBear :: Text
  , addrMorphoMarketBull :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON Addresses where
  parseJSON = withObject "Addresses" $ \v ->
    Addresses
      <$> v .: "USDC"
      <*> v .: "DXY_BEAR"
      <*> v .: "DXY_BULL"
      <*> v .: "SDXY_BEAR"
      <*> v .: "SDXY_BULL"
      <*> v .: "SYNTHETIC_SPLITTER"
      <*> v .: "CURVE_POOL"
      <*> v .: "ZAP_ROUTER"
      <*> v .: "LEVERAGE_ROUTER"
      <*> v .: "BULL_LEVERAGE_ROUTER"
      <*> v .: "STAKING_BEAR"
      <*> v .: "STAKING_BULL"
      <*> v .: "BASKET_ORACLE"
      <*> v .: "MOCK_ADAPTER"
      <*> v .: "MORPHO_ORACLE_BEAR"
      <*> v .: "MORPHO_ORACLE_BULL"
      <*> v .: "STAKED_ORACLE_BEAR"
      <*> v .: "STAKED_ORACLE_BULL"
      <*> v .: "MORPHO"
      <*> v .: "MORPHO_MARKET_BEAR"
      <*> v .: "MORPHO_MARKET_BULL"

data Deployment = Deployment
  { deployDate :: Text
  , deployAddresses :: Addresses
  }
  deriving stock (Show)

instance FromJSON Deployment where
  parseJSON = withObject "Deployment" $ \v ->
    Deployment
      <$> v .: "RELEASE_DATE"
      <*> parseJSON (Object v)

currentAddresses :: [Deployment] -> Addresses
currentAddresses = deployAddresses . head . sortBy (comparing (Down . deployDate))

loadDeployments :: FilePath -> IO (Either String [Deployment])
loadDeployments = eitherDecodeFileStrict

loadConfig :: IO (Either String Config)
loadConfig = do
  mRpcUrl <- firstEnv ["RPC_URL", "PERPS_RPC_URL"]
  case mRpcUrl of
    Nothing -> pure $ Left "RPC_URL or PERPS_RPC_URL environment variable not set"
    Just rpcUrl -> do
      chainIdStr <- fromMaybe "11155111" <$> lookupEnv "CHAIN_ID"
      portStr <- fromMaybe "3001" <$> lookupEnv "PORT"
      corsStr <- fromMaybe "http://localhost:5173" <$> lookupEnv "CORS_ORIGINS"
      mDatabaseUrl <- lookupEnv "DATABASE_URL"
      indexerBlockStr <- fromMaybe "0" <$> lookupEnv "INDEXER_START_BLOCK"
      pythBenchmarksUrl <- fromMaybe "https://benchmarks.pyth.network" <$> lookupEnv "PYTH_BENCHMARKS_URL"
      pythHermesUrl <- fromMaybe "https://hermes.pyth.network" <$> lookupEnv "PYTH_HERMES_URL"
      mPythApiKey <- lookupEnv "PYTH_API_KEY"
      pythBackfillDaysStr <- fromMaybe "7" <$> lookupEnv "PYTH_BACKFILL_DAYS"
      pythSampleIntervalStr <- fromMaybe "60" <$> lookupEnv "PYTH_SAMPLE_INTERVAL_SECONDS"
      pythIngestionStr <- fromMaybe "false" <$> lookupEnv "PYTH_INGESTION_ENABLED"
      perpsRpcUrl <- fromMaybe rpcUrl <$> lookupEnv "PERPS_RPC_URL"
      perpsChainIdStr <- fromMaybe "421614" <$> lookupEnv "PERPS_CHAIN_ID"
      perpsUsdc <- fromMaybe "0xf1e1B188b87525C51ECe4bae8627ae621D769651" <$> lookupEnv "PERPS_USDC"
      perpsOrderRouter <- fromMaybe "0x4A0a6c028164A1254e10C3e39cc89Af45090069e" <$> lookupEnv "PERPS_ORDER_ROUTER"
      perpsPletherOracle <- fromMaybe "0x8c95f554D728215b9f8D15b5F3Da5F5CD7Ba08bA" <$> lookupEnv "PERPS_PLETHER_ORACLE"
      perpsIndexerStartBlockStr <- fromMaybe "273137426" <$> lookupEnv "PERPS_INDEXER_START_BLOCK"
      mFaucetPrivateKey <- lookupEnv "FAUCET_PRIVATE_KEY"
      mKeeperPrivateKey <- lookupEnv "KEEPER_PRIVATE_KEY"
      keeperPollSecondsStr <- fromMaybe "1" <$> lookupEnv "KEEPER_POLL_SECONDS"
      keeperMaxBatchSizeStr <- fromMaybe "20" <$> lookupEnv "KEEPER_MAX_BATCH_SIZE"
      keeperConfirmationsStr <- fromMaybe "1" <$> lookupEnv "KEEPER_CONFIRMATIONS"
      keeperGasBufferBpsStr <- fromMaybe "2000" <$> lookupEnv "KEEPER_GAS_BUFFER_BPS"
      keeperFeeBufferBpsStr <- fromMaybe "2500" <$> lookupEnv "KEEPER_FEE_BUFFER_BPS"

      let chainId = fromMaybe 11155111 (readMaybe chainIdStr)
          indexerStartBlock = fromMaybe 0 (readMaybe indexerBlockStr)
          port = fromMaybe 3001 (readMaybe portStr)
          corsOrigins = filter (not . T.null) $ map T.strip $ T.splitOn " " $ T.pack corsStr
          pythBackfillDays = fromMaybe 7 (readMaybe pythBackfillDaysStr)
          pythSampleIntervalSeconds = fromMaybe 60 (readMaybe pythSampleIntervalStr)
          pythIngestionEnabled = parseBool pythIngestionStr
          perpsChainId = fromMaybe 421614 (readMaybe perpsChainIdStr)
          perpsIndexerStartBlock = fromMaybe 0 (readMaybe perpsIndexerStartBlockStr)
          keeperPollSeconds = fromMaybe 1 (readMaybe keeperPollSecondsStr)
          keeperMaxBatchSize = fromMaybe 20 (readMaybe keeperMaxBatchSizeStr)
          keeperConfirmations = fromMaybe 1 (readMaybe keeperConfirmationsStr)
          keeperGasBufferBps = fromMaybe 2000 (readMaybe keeperGasBufferBpsStr)
          keeperFeeBufferBps = fromMaybe 2500 (readMaybe keeperFeeBufferBpsStr)
          addressFile = case chainId of
            1 -> "config/addresses.mainnet.json"
            11155111 -> "config/addresses.sepolia.json"
            31337 -> "config/addresses.local.json"
            _ -> "config/addresses.sepolia.json"

      eDeployments <- loadDeployments addressFile
      case eDeployments of
        Left err -> pure $ Left $ "Failed to load addresses: " <> err
        Right deployments ->
          pure $
            Right $
              Config
                { cfgRpcUrl = T.pack rpcUrl
                , cfgChainId = chainId
                , cfgPort = port
                , cfgCorsOrigins = corsOrigins
                , cfgDeployments = deployments
                , cfgDatabaseUrl = fmap T.pack mDatabaseUrl
                , cfgIndexerStartBlock = indexerStartBlock
                , cfgPythBenchmarksUrl = T.pack pythBenchmarksUrl
                , cfgPythHermesUrl = T.pack pythHermesUrl
                , cfgPythApiKey = fmap T.pack mPythApiKey
                , cfgPythBackfillDays = max 1 pythBackfillDays
                , cfgPythSampleIntervalSeconds = max 60 pythSampleIntervalSeconds
                , cfgPythIngestionEnabled = pythIngestionEnabled
                , cfgPerpsRpcUrl = T.pack perpsRpcUrl
                , cfgPerpsChainId = perpsChainId
                , cfgPerpsUsdc = T.pack perpsUsdc
                , cfgPerpsOrderRouter = T.pack perpsOrderRouter
                , cfgPerpsPletherOracle = T.pack perpsPletherOracle
                , cfgPerpsIndexerStartBlock = perpsIndexerStartBlock
                , cfgFaucetPrivateKey = fmap T.pack mFaucetPrivateKey
                , cfgKeeperPrivateKey = fmap T.pack mKeeperPrivateKey
                , cfgKeeperPollSeconds = max 1 keeperPollSeconds
                , cfgKeeperMaxBatchSize = max 1 keeperMaxBatchSize
                , cfgKeeperConfirmations = max 0 keeperConfirmations
                , cfgKeeperGasBufferBps = max 0 keeperGasBufferBps
                , cfgKeeperFeeBufferBps = max 0 keeperFeeBufferBps
                }

firstEnv :: [String] -> IO (Maybe String)
firstEnv [] = pure Nothing
firstEnv (name : rest) = do
  value <- lookupEnv name
  case value of
    Just found | not (null found) -> pure $ Just found
    _ -> firstEnv rest

parseBool :: String -> Bool
parseBool value =
  case T.toLower (T.pack value) of
    "0" -> False
    "false" -> False
    "no" -> False
    "off" -> False
    _ -> True
