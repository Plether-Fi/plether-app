module Plether.Config
  ( Config (..)
  , AaConfig (..)
  , Addresses (..)
  , Deployment (..)
  , loadConfig
  , loadDeployments
  , currentAddresses
  , defaultPythHermesUrl
  , defaultPythLatestMaxAgeSeconds
  , maxPythLatestMaxAgeSeconds
  , validatePythLatestMaxAgeSeconds
  ) where

import Data.Aeson (FromJSON (..), Value (..), eitherDecodeFileStrict, withObject, (.:))
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Plether.Utils.Address (isValidAddress)
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
  , cfgPythLatestMaxAgeSeconds :: Integer
  , cfgPythIngestionEnabled :: Bool
  , cfgPerpsRpcUrl :: Text
  , cfgPerpsChainId :: Integer
  , cfgPerpsUsdc :: Text
  , cfgPerpsOrderRouter :: Text
  , cfgPerpsCfdEngine :: Text
  , cfgPerpsMarginClearinghouse :: Text
  , cfgPerpsPletherOracle :: Text
  , cfgPerpsAccountLens :: Text
  , cfgPerpsIndexerStartBlock :: Integer
  , cfgAaConfig :: Maybe AaConfig
  , cfgFaucetPrivateKey :: Maybe Text
  , cfgKeeperPrivateKey :: Maybe Text
  , cfgKeeperPollSeconds :: Int
  , cfgKeeperMaxBatchSize :: Int
  , cfgKeeperConfirmations :: Int
  , cfgKeeperGasBufferBps :: Integer
  , cfgKeeperFeeBufferBps :: Integer
  }
  deriving stock (Show)

data AaConfig = AaConfig
  { aaProxyOriginToken :: Text
  , aaPimlicoApiKey :: Text
  , aaSponsorshipPolicyId :: Text
  , aaSponsorshipEnabled :: Bool
  , aaIpRateLimitPerMinute :: Int
  , aaAccountRateLimitPerMinute :: Int
  , aaMaxRequestBytes :: Int
  , aaSponsoredGasAlertWeiPerHour :: Integer
  }

instance Show AaConfig where
  show cfg =
    "AaConfig {aaProxyOriginToken = <redacted>, aaPimlicoApiKey = <redacted>, "
      <> "aaSponsorshipPolicyId = <redacted>, aaSponsorshipEnabled = "
      <> show (aaSponsorshipEnabled cfg)
      <> ", aaIpRateLimitPerMinute = "
      <> show (aaIpRateLimitPerMinute cfg)
      <> ", aaAccountRateLimitPerMinute = "
      <> show (aaAccountRateLimitPerMinute cfg)
      <> ", aaMaxRequestBytes = "
      <> show (aaMaxRequestBytes cfg)
      <> ", aaSponsoredGasAlertWeiPerHour = "
      <> show (aaSponsoredGasAlertWeiPerHour cfg)
      <> "}"

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

defaultPythHermesUrl :: Text
defaultPythHermesUrl = "https://pyth.dourolabs.app/hermes"

defaultPythLatestMaxAgeSeconds :: Integer
defaultPythLatestMaxAgeSeconds = 10

maxPythLatestMaxAgeSeconds :: Integer
maxPythLatestMaxAgeSeconds = 10

validatePythLatestMaxAgeSeconds :: String -> Either String Integer
validatePythLatestMaxAgeSeconds rawValue =
  case readMaybe normalizedValue of
    Just seconds
      | show seconds == normalizedValue
      , seconds >= 1 && seconds <= maxPythLatestMaxAgeSeconds -> Right seconds
    _ ->
      Left $
        "PYTH_LATEST_MAX_AGE_SECONDS must be a whole number between 1 and "
          <> show maxPythLatestMaxAgeSeconds
          <> "; the upper bound preserves headroom below the oracle's 15-second staleness limit"
 where
  normalizedValue = T.unpack $ T.strip $ T.pack rawValue

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
      pythHermesUrl <- fromMaybe (T.unpack defaultPythHermesUrl) <$> lookupEnv "PYTH_HERMES_URL"
      mPythApiKey <- lookupEnv "PYTH_API_KEY"
      pythBackfillDaysStr <- fromMaybe "7" <$> lookupEnv "PYTH_BACKFILL_DAYS"
      pythSampleIntervalStr <- fromMaybe "60" <$> lookupEnv "PYTH_SAMPLE_INTERVAL_SECONDS"
      pythLatestMaxAgeStr <- fromMaybe (show defaultPythLatestMaxAgeSeconds) <$> lookupEnv "PYTH_LATEST_MAX_AGE_SECONDS"
      pythIngestionStr <- fromMaybe "false" <$> lookupEnv "PYTH_INGESTION_ENABLED"
      perpsRpcUrl <- fromMaybe rpcUrl <$> lookupEnv "PERPS_RPC_URL"
      perpsChainIdStr <- fromMaybe "421614" <$> lookupEnv "PERPS_CHAIN_ID"
      perpsAccountLens <- fromMaybe "0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1" <$> lookupEnv "PERPS_ACCOUNT_LENS"
      perpsUsdc <- fromMaybe "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3" <$> lookupEnv "PERPS_USDC"
      perpsOrderRouter <- fromMaybe "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E" <$> lookupEnv "PERPS_ORDER_ROUTER"
      perpsCfdEngine <- fromMaybe "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a" <$> lookupEnv "PERPS_CFD_ENGINE"
      perpsMarginClearinghouse <- fromMaybe "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e" <$> lookupEnv "PERPS_MARGIN_CLEARINGHOUSE"
      perpsPletherOracle <- fromMaybe "0xADfEd3bf768D810309B97b4dF9F9E77Eaa3a401c" <$> lookupEnv "PERPS_PLETHER_ORACLE"
      perpsIndexerStartBlockStr <- fromMaybe "288439939" <$> lookupEnv "PERPS_INDEXER_START_BLOCK"
      mAaProxyOriginToken <- firstEnv ["AA_PROXY_ORIGIN_TOKEN"]
      mPimlicoApiKey <- firstEnv ["PIMLICO_API_KEY"]
      mPimlicoPolicyId <- firstEnv ["PIMLICO_SPONSORSHIP_POLICY_ID"]
      aaSponsorshipEnabledStr <- fromMaybe "true" <$> lookupEnv "AA_SPONSORSHIP_ENABLED"
      aaIpRateLimitStr <- fromMaybe "120" <$> lookupEnv "AA_IP_RATE_LIMIT_PER_MINUTE"
      aaAccountRateLimitStr <- fromMaybe "30" <$> lookupEnv "AA_ACCOUNT_RATE_LIMIT_PER_MINUTE"
      aaMaxRequestBytesStr <- fromMaybe "262144" <$> lookupEnv "AA_MAX_REQUEST_BYTES"
      aaSponsoredGasAlertWeiStr <- fromMaybe "0" <$> lookupEnv "AA_SPONSORED_GAS_ALERT_WEI_PER_HOUR"
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
          aaIpRateLimit = fromMaybe 120 (readMaybe aaIpRateLimitStr)
          aaAccountRateLimit = fromMaybe 30 (readMaybe aaAccountRateLimitStr)
          aaMaxRequestBytes = fromMaybe 262144 (readMaybe aaMaxRequestBytesStr)
          aaSponsoredGasAlertWei = fromMaybe 0 (readMaybe aaSponsoredGasAlertWeiStr)
          keeperPollSeconds = fromMaybe 1 (readMaybe keeperPollSecondsStr)
          keeperMaxBatchSize = fromMaybe 20 (readMaybe keeperMaxBatchSizeStr)
          keeperConfirmations = fromMaybe 1 (readMaybe keeperConfirmationsStr)
          keeperGasBufferBps = fromMaybe 2000 (readMaybe keeperGasBufferBpsStr)
          keeperFeeBufferBps = fromMaybe 2500 (readMaybe keeperFeeBufferBpsStr)
          addressFile = case chainId of
            1 -> "config/addresses.mainnet.json"
            11155111 -> "config/addresses.sepolia.json"
            421614 -> "config/addresses.arbitrum-sepolia.json"
            31337 -> "config/addresses.local.json"
            _ -> "config/addresses.sepolia.json"
          aaConfig =
            case
              ( parseBoolStrict aaSponsorshipEnabledStr
              , mAaProxyOriginToken
              , mPimlicoApiKey
              , mPimlicoPolicyId
              )
            of
              (Nothing, _, _, _) ->
                Left
                  "AA_SPONSORSHIP_ENABLED must be one of true, false, 1, 0, yes, no, on, or off"
              (Just _, Nothing, Nothing, Nothing) -> Right Nothing
              (Just aaSponsorshipEnabled, Just originToken, Just apiKey, Just policyId)
                | not $
                    validAaDeploymentAddresses
                      perpsUsdc
                      perpsOrderRouter
                      perpsCfdEngine
                      perpsMarginClearinghouse ->
                    Left
                      "Managed AA sponsorship requires the reviewed Arbitrum Sepolia \
                      \PERPS_USDC, PERPS_ORDER_ROUTER, PERPS_CFD_ENGINE, and \
                      \PERPS_MARGIN_CLEARINGHOUSE deployment addresses"
                | perpsChainId /= 421614 ->
                    Left "Managed AA sponsorship is supported only on PERPS_CHAIN_ID=421614"
                | otherwise ->
                    Right $
                      Just $
                        AaConfig
                          { aaProxyOriginToken = T.pack originToken
                          , aaPimlicoApiKey = T.pack apiKey
                          , aaSponsorshipPolicyId = T.pack policyId
                          , aaSponsorshipEnabled = aaSponsorshipEnabled
                          , aaIpRateLimitPerMinute = max 1 aaIpRateLimit
                          , aaAccountRateLimitPerMinute = max 1 aaAccountRateLimit
                          , aaMaxRequestBytes = max 1024 aaMaxRequestBytes
                          , aaSponsoredGasAlertWeiPerHour = max 0 aaSponsoredGasAlertWei
                          }
              _ ->
                Left
                  "AA proxy configuration is partial; set all of AA_PROXY_ORIGIN_TOKEN, \
                  \PIMLICO_API_KEY, and PIMLICO_SPONSORSHIP_POLICY_ID"

      case (validatePythLatestMaxAgeSeconds pythLatestMaxAgeStr, aaConfig) of
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err
        (Right pythLatestMaxAgeSeconds, Right resolvedAaConfig) -> do
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
                , cfgPythApiKey = nonBlankText mPythApiKey
                , cfgPythBackfillDays = max 1 pythBackfillDays
                , cfgPythSampleIntervalSeconds = max 60 pythSampleIntervalSeconds
                , cfgPythLatestMaxAgeSeconds = pythLatestMaxAgeSeconds
                , cfgPythIngestionEnabled = pythIngestionEnabled
                , cfgPerpsRpcUrl = T.pack perpsRpcUrl
                , cfgPerpsChainId = perpsChainId
                , cfgPerpsUsdc = T.pack perpsUsdc
                , cfgPerpsOrderRouter = T.pack perpsOrderRouter
                , cfgPerpsCfdEngine = T.pack perpsCfdEngine
                , cfgPerpsMarginClearinghouse = T.pack perpsMarginClearinghouse
                , cfgPerpsPletherOracle = T.pack perpsPletherOracle
                , cfgPerpsAccountLens = T.pack perpsAccountLens
                , cfgPerpsIndexerStartBlock = perpsIndexerStartBlock
                , cfgAaConfig = resolvedAaConfig
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

nonBlankText :: Maybe String -> Maybe Text
nonBlankText = \case
  Just value | not (T.null stripped) -> Just stripped
   where
    stripped = T.strip $ T.pack value
  _ -> Nothing

parseBool :: String -> Bool
parseBool value =
  case T.toLower (T.pack value) of
    "0" -> False
    "false" -> False
    "no" -> False
    "off" -> False
    _ -> True

parseBoolStrict :: String -> Maybe Bool
parseBoolStrict value =
  case T.toLower $ T.strip $ T.pack value of
    "1" -> Just True
    "true" -> Just True
    "yes" -> Just True
    "on" -> Just True
    "0" -> Just False
    "false" -> Just False
    "no" -> Just False
    "off" -> Just False
    _ -> Nothing

validAaDeploymentAddresses :: String -> String -> String -> String -> Bool
validAaDeploymentAddresses usdc router engine clearinghouse =
  and
    [ reviewed usdc "0xb15503d70b0eaa644dc6650d2a248762f7c5bce3"
    , reviewed router "0x04e3103752f623fbcdcd01f588590af4c53e4c1e"
    , reviewed engine "0x6a25ea1015b5f032d8a2d95d57aefcb99219bf0a"
    , reviewed clearinghouse "0x19c2f60f6312eaf9acde4c2b04551a05ca9be76e"
    ]
  where
    reviewed raw expected =
      let value = T.toLower $ T.strip $ T.pack raw
       in isValidAddress value && value == expected
