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
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
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
  mRpcUrl <- lookupEnv "RPC_URL"
  case mRpcUrl of
    Nothing -> pure $ Left "RPC_URL environment variable not set"
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

      let chainId = fromMaybe 11155111 (readMaybe chainIdStr)
          indexerStartBlock = fromMaybe 0 (readMaybe indexerBlockStr)
          port = fromMaybe 3001 (readMaybe portStr)
          corsOrigins = filter (not . T.null) $ map T.strip $ T.splitOn " " $ T.pack corsStr
          pythBackfillDays = fromMaybe 7 (readMaybe pythBackfillDaysStr)
          pythSampleIntervalSeconds = fromMaybe 60 (readMaybe pythSampleIntervalStr)
          pythIngestionEnabled = parseBool pythIngestionStr
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
                }

parseBool :: String -> Bool
parseBool value =
  case T.toLower (T.pack value) of
    "0" -> False
    "false" -> False
    "no" -> False
    "off" -> False
    _ -> True
