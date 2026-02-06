module Plether.Config
  ( Config (..)
  , Addresses (..)
  , loadConfig
  , loadAddresses
  ) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:))
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
  , cfgAddresses :: Addresses
  , cfgDatabaseUrl :: Maybe Text
  , cfgIndexerStartBlock :: Integer
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

loadAddresses :: FilePath -> IO (Either String Addresses)
loadAddresses = eitherDecodeFileStrict

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

      let chainId = fromMaybe 11155111 (readMaybe chainIdStr)
          indexerStartBlock = fromMaybe 0 (readMaybe indexerBlockStr)
          port = fromMaybe 3001 (readMaybe portStr)
          corsOrigins = filter (not . T.null) $ map T.strip $ T.splitOn " " $ T.pack corsStr
          addressFile = case chainId of
            1 -> "config/addresses.mainnet.json"
            11155111 -> "config/addresses.sepolia.json"
            31337 -> "config/addresses.local.json"  -- Local Anvil deployment
            _ -> "config/addresses.sepolia.json"

      eAddresses <- loadAddresses addressFile
      case eAddresses of
        Left err -> pure $ Left $ "Failed to load addresses: " <> err
        Right addresses ->
          pure $
            Right $
              Config
                { cfgRpcUrl = T.pack rpcUrl
                , cfgChainId = chainId
                , cfgPort = port
                , cfgCorsOrigins = corsOrigins
                , cfgAddresses = addresses
                , cfgDatabaseUrl = fmap T.pack mDatabaseUrl
                , cfgIndexerStartBlock = indexerStartBlock
                }
