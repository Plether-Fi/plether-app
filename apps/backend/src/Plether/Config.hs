module Plether.Config
  ( Config (..)
  , AaConfig (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  , Addresses (..)
  , Deployment (..)
  , loadConfig
  , loadDeployments
  , currentAddresses
  , defaultPythHermesUrl
  , defaultPythLatestMaxAgeSeconds
  , maxPythLatestMaxAgeSeconds
  , validatePythLatestMaxAgeSeconds
  , parsePerpsCandleReadIntervals
  , parsePerpsCandleReadMode
  , parsePerpsCandleWriteMode
  , perpsCandleRollupReadEnabled
  , validatePerpsCandleModeCombination
  ) where

import Data.Aeson (FromJSON (..), Value (..), eitherDecodeFileStrict, withObject, (.:))
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Plether.Utils.Address (isValidAddress)
import Plether.Types.Perps (canonicalBasketCandleIntervals)
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
  , cfgPerpsCandleWriteMode :: PerpsCandleWriteMode
  , cfgPerpsCandleReadMode :: PerpsCandleReadMode
  , cfgPerpsCandleReadIntervals :: [Integer]
  , cfgPerpsCandleShadowSampleBps :: Int
  , cfgPerpsCandleStrictCoverage :: Bool
  , cfgPerpsCandleLatenessSeconds :: Integer
  , cfgPerpsCandleFinalizationGraceSeconds :: Integer
  , cfgPerpsRpcUrl :: Text
  , cfgPerpsChainId :: Integer
  , cfgPerpsUsdc :: Text
  , cfgPerpsOrderRouter :: Text
  , cfgPerpsCfdEngine :: Text
  , cfgPerpsMarginClearinghouse :: Text
  , cfgPerpsPletherOracle :: Text
  , cfgPerpsAccountLens :: Text
  , cfgPerpsHousePool :: Text
  , cfgPerpsSettlementMonitorLens :: Text
  , cfgPerpsIndexerStartBlock :: Integer
  , cfgVaultHistoryHousePoolAddress :: Text
  , cfgVaultHistorySeniorVaultAddress :: Text
  , cfgVaultHistoryJuniorVaultAddress :: Text
  , cfgVaultHistoryDeploymentBlock :: Integer
  , cfgVaultHistoryRpcUrl :: Text
  , cfgVaultHistoryConfirmations :: Integer
  , cfgAaConfig :: Maybe AaConfig
  , cfgFaucetPrivateKey :: Maybe Text
  , cfgKeeperPrivateKey :: Maybe Text
  , cfgKeeperPollSeconds :: Int
  , cfgKeeperMaxBatchSize :: Int
  , cfgKeeperConfirmations :: Int
  , cfgKeeperGasBufferBps :: Integer
  , cfgKeeperFeeBufferBps :: Integer
  , cfgLpSettlementEnabled :: Bool
  , cfgLpSettlementPollSeconds :: Int
  }
  deriving stock (Show)

data PerpsCandleWriteMode
  = PerpsCandleWritesOff
  | PerpsCandleWritesDual
  deriving stock (Eq, Show)

data PerpsCandleReadMode
  = PerpsCandleReadsLegacy
  | PerpsCandleReadsShadow
  | PerpsCandleReadsRollup
  deriving stock (Eq, Show)

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
      candleWriteModeStr <- fromMaybe "off" <$> lookupEnv "PERPS_CANDLE_WRITE_MODE"
      candleReadModeStr <- fromMaybe "legacy" <$> lookupEnv "PERPS_CANDLE_READ_MODE"
      candleReadIntervalsStr <- fromMaybe "" <$> lookupEnv "PERPS_CANDLE_READ_INTERVALS"
      candleShadowSampleBpsStr <- fromMaybe "0" <$> lookupEnv "PERPS_CANDLE_SHADOW_SAMPLE_BPS"
      candleStrictCoverageStr <- fromMaybe "true" <$> lookupEnv "PERPS_CANDLE_STRICT_COVERAGE"
      candleLatenessSecondsStr <- fromMaybe "120" <$> lookupEnv "PERPS_CANDLE_LATENESS_SECONDS"
      candleFinalizationGraceSecondsStr <-
        fromMaybe "15" <$> lookupEnv "PERPS_CANDLE_FINALIZATION_GRACE_SECONDS"
      perpsRpcUrl <- fromMaybe rpcUrl <$> lookupEnv "PERPS_RPC_URL"
      perpsChainIdStr <- fromMaybe "421614" <$> lookupEnv "PERPS_CHAIN_ID"
      perpsAccountLens <- fromMaybe "0x429DA61a7a616DeDD84d2a51eB6Dc1bD72427dC1" <$> lookupEnv "PERPS_ACCOUNT_LENS"
      perpsUsdc <- fromMaybe "0x1647e41f49ED6D688936092B5a291c4B28106343" <$> lookupEnv "PERPS_USDC"
      perpsOrderRouter <- fromMaybe "0x97A901dE2B267c307E264FD5F71403F8072F73e7" <$> lookupEnv "PERPS_ORDER_ROUTER"
      perpsCfdEngine <- fromMaybe "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D" <$> lookupEnv "PERPS_CFD_ENGINE"
      perpsMarginClearinghouse <- fromMaybe "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211" <$> lookupEnv "PERPS_MARGIN_CLEARINGHOUSE"
      perpsPletherOracle <- fromMaybe "0xC69ec16EfB71F62984E9b2688396F34062277FdC" <$> lookupEnv "PERPS_PLETHER_ORACLE"
      perpsHousePool <- fromMaybe "0x86939a377A78EDe8EEe5445765ac77c9016E35E2" <$> lookupEnv "PERPS_HOUSE_POOL"
      perpsSettlementMonitorLens <- fromMaybe "0xd251AC0BD90780c48F31F575152808315200664E" <$> lookupEnv "PERPS_SETTLEMENT_MONITOR_LENS"
      perpsIndexerStartBlockStr <- fromMaybe "302257125" <$> lookupEnv "PERPS_INDEXER_START_BLOCK"
      vaultHistoryHousePool <- fromMaybe "0x86939a377A78EDe8EEe5445765ac77c9016E35E2" <$> lookupEnv "VAULT_HISTORY_HOUSE_POOL_ADDRESS"
      vaultHistorySeniorVault <- fromMaybe "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710" <$> lookupEnv "VAULT_HISTORY_SENIOR_VAULT_ADDRESS"
      vaultHistoryJuniorVault <- fromMaybe "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb" <$> lookupEnv "VAULT_HISTORY_JUNIOR_VAULT_ADDRESS"
      vaultHistoryDeploymentBlockStr <- fromMaybe "302257125" <$> lookupEnv "VAULT_HISTORY_DEPLOYMENT_BLOCK"
      mVaultHistoryRpcUrl <- lookupEnv "VAULT_HISTORY_RPC_URL"
      vaultHistoryConfirmationsStr <- fromMaybe "12" <$> lookupEnv "VAULT_HISTORY_CONFIRMATIONS"
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
      lpSettlementEnabledStr <- fromMaybe "false" <$> lookupEnv "LP_SETTLEMENT_ENABLED"
      lpSettlementPollSecondsStr <- fromMaybe "15" <$> lookupEnv "LP_SETTLEMENT_POLL_SECONDS"

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

          candleConfig = do
            writeMode <- parsePerpsCandleWriteMode candleWriteModeStr
            readMode <- parsePerpsCandleReadMode candleReadModeStr
            readIntervals <- parsePerpsCandleReadIntervals candleReadIntervalsStr
            shadowSampleBps <-
              parseBoundedWholeNumber
                "PERPS_CANDLE_SHADOW_SAMPLE_BPS"
                0
                10_000
                candleShadowSampleBpsStr
            strictCoverage <-
              maybe
                (Left "PERPS_CANDLE_STRICT_COVERAGE must be a boolean")
                Right
                (parseBoolStrict candleStrictCoverageStr)
            latenessSeconds <-
              parseBoundedWholeNumber
                "PERPS_CANDLE_LATENESS_SECONDS"
                0
                86_400
                candleLatenessSecondsStr
            finalizationGraceSeconds <-
              parseBoundedWholeNumber
                "PERPS_CANDLE_FINALIZATION_GRACE_SECONDS"
                0
                60
                candleFinalizationGraceSecondsStr
            validatePerpsCandleModeCombination
              writeMode
              readMode
              readIntervals
              strictCoverage
            pure
              ( writeMode
              , readMode
              , readIntervals
              , shadowSampleBps
              , strictCoverage
              , fromIntegral latenessSeconds
              , fromIntegral finalizationGraceSeconds
              )

          vaultHistoryConfig = do
            deploymentBlock <-
              parseNonNegativeInteger
                "VAULT_HISTORY_DEPLOYMENT_BLOCK"
                vaultHistoryDeploymentBlockStr
            confirmations <-
              parseNonNegativeInteger
                "VAULT_HISTORY_CONFIRMATIONS"
                vaultHistoryConfirmationsStr
            let addresses =
                  [ ("VAULT_HISTORY_HOUSE_POOL_ADDRESS", vaultHistoryHousePool)
                  , ("VAULT_HISTORY_SENIOR_VAULT_ADDRESS", vaultHistorySeniorVault)
                  , ("VAULT_HISTORY_JUNIOR_VAULT_ADDRESS", vaultHistoryJuniorVault)
                  ]
            case [name | (name, address) <- addresses, not $ isCanonicalVaultAddress address] of
              invalid : _ -> Left $ invalid <> " must be a valid Ethereum address"
              [] ->
                Right
                  ( deploymentBlock
                  , confirmations
                  , fromMaybe (T.pack perpsRpcUrl) $ nonBlankText mVaultHistoryRpcUrl
                  )

          lpSettlementConfig = do
            enabled <-
              maybe
                (Left "LP_SETTLEMENT_ENABLED must be a boolean")
                Right
                (parseBoolStrict lpSettlementEnabledStr)
            pollSeconds <-
              parseBoundedWholeNumber
                "LP_SETTLEMENT_POLL_SECONDS"
                1
                3_600
                lpSettlementPollSecondsStr
            case
                [ name
                | (name, address) <-
                    [ ("PERPS_HOUSE_POOL", perpsHousePool)
                    , ("PERPS_SETTLEMENT_MONITOR_LENS", perpsSettlementMonitorLens)
                    ]
                , not $ isCanonicalVaultAddress address
                ]
              of
              invalid : _ -> Left $ invalid <> " must be a valid Ethereum address"
              []
                | T.toLower (T.strip $ T.pack perpsSettlementMonitorLens)
                    == "0xe1fc0a465dabdfd8ee33d4aa960108f800b3f151" ->
                    Left "PERPS_SETTLEMENT_MONITOR_LENS must be the facade, not the v1.2.0 monitor sidecar"
                | otherwise -> Right (enabled, pollSeconds)

      case (validatePythLatestMaxAgeSeconds pythLatestMaxAgeStr, aaConfig, candleConfig, vaultHistoryConfig, lpSettlementConfig) of
        (Left err, _, _, _, _) -> pure $ Left err
        (_, Left err, _, _, _) -> pure $ Left err
        (_, _, Left err, _, _) -> pure $ Left err
        (_, _, _, Left err, _) -> pure $ Left err
        (_, _, _, _, Left err) -> pure $ Left err
        ( Right pythLatestMaxAgeSeconds
          , Right resolvedAaConfig
          , Right
              ( candleWriteMode
                , candleReadMode
                , candleReadIntervals
                , candleShadowSampleBps
                , candleStrictCoverage
                , candleLatenessSeconds
                , candleFinalizationGraceSeconds
                )
          , Right
              ( vaultHistoryDeploymentBlock
                , vaultHistoryConfirmations
                , vaultHistoryRpcUrl
                )
          , Right (lpSettlementEnabled, lpSettlementPollSeconds)
          ) -> do
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
                , cfgPerpsCandleWriteMode = candleWriteMode
                , cfgPerpsCandleReadMode = candleReadMode
                , cfgPerpsCandleReadIntervals = candleReadIntervals
                , cfgPerpsCandleShadowSampleBps = candleShadowSampleBps
                , cfgPerpsCandleStrictCoverage = candleStrictCoverage
                , cfgPerpsCandleLatenessSeconds = candleLatenessSeconds
                , cfgPerpsCandleFinalizationGraceSeconds = candleFinalizationGraceSeconds
                , cfgPerpsRpcUrl = T.pack perpsRpcUrl
                , cfgPerpsChainId = perpsChainId
                , cfgPerpsUsdc = T.pack perpsUsdc
                , cfgPerpsOrderRouter = T.pack perpsOrderRouter
                , cfgPerpsCfdEngine = T.pack perpsCfdEngine
                , cfgPerpsMarginClearinghouse = T.pack perpsMarginClearinghouse
                , cfgPerpsPletherOracle = T.pack perpsPletherOracle
                , cfgPerpsAccountLens = T.pack perpsAccountLens
                , cfgPerpsHousePool = T.pack perpsHousePool
                , cfgPerpsSettlementMonitorLens = T.pack perpsSettlementMonitorLens
                , cfgPerpsIndexerStartBlock = perpsIndexerStartBlock
                , cfgVaultHistoryHousePoolAddress = T.pack vaultHistoryHousePool
                , cfgVaultHistorySeniorVaultAddress = T.pack vaultHistorySeniorVault
                , cfgVaultHistoryJuniorVaultAddress = T.pack vaultHistoryJuniorVault
                , cfgVaultHistoryDeploymentBlock = vaultHistoryDeploymentBlock
                , cfgVaultHistoryRpcUrl = vaultHistoryRpcUrl
                , cfgVaultHistoryConfirmations = vaultHistoryConfirmations
                , cfgAaConfig = resolvedAaConfig
                , cfgFaucetPrivateKey = fmap T.pack mFaucetPrivateKey
                , cfgKeeperPrivateKey = fmap T.pack mKeeperPrivateKey
                , cfgKeeperPollSeconds = max 1 keeperPollSeconds
                , cfgKeeperMaxBatchSize = max 1 keeperMaxBatchSize
                , cfgKeeperConfirmations = max 0 keeperConfirmations
                , cfgKeeperGasBufferBps = max 0 keeperGasBufferBps
                , cfgKeeperFeeBufferBps = max 0 keeperFeeBufferBps
                , cfgLpSettlementEnabled = lpSettlementEnabled
                , cfgLpSettlementPollSeconds = lpSettlementPollSeconds
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

parsePerpsCandleWriteMode :: String -> Either String PerpsCandleWriteMode
parsePerpsCandleWriteMode raw =
  case T.toLower $ T.strip $ T.pack raw of
    "off" -> Right PerpsCandleWritesOff
    "dual" -> Right PerpsCandleWritesDual
    _ -> Left "PERPS_CANDLE_WRITE_MODE must be one of off or dual"

parsePerpsCandleReadMode :: String -> Either String PerpsCandleReadMode
parsePerpsCandleReadMode raw =
  case T.toLower $ T.strip $ T.pack raw of
    "legacy" -> Right PerpsCandleReadsLegacy
    "shadow" -> Right PerpsCandleReadsShadow
    "rollup" -> Right PerpsCandleReadsRollup
    _ -> Left "PERPS_CANDLE_READ_MODE must be one of legacy, shadow, or rollup"

parsePerpsCandleReadIntervals :: String -> Either String [Integer]
parsePerpsCandleReadIntervals raw =
  traverse parseInterval tokens
  where
    tokens =
      filter (not . null)
        $ map T.unpack
        $ concatMap T.words
        $ T.splitOn ","
        $ T.strip
        $ T.pack raw
    parseInterval token =
      case readMaybe token of
        Just interval | interval `elem` canonicalBasketCandleIntervals -> Right interval
        _ ->
          Left
            "PERPS_CANDLE_READ_INTERVALS may contain only 60, 180, 300, 900, 1800, 3600, or 86400"

-- | Rollup HTTP reads are fail-closed. Shadow mode is reserved (and therefore
-- never enables a public rollup route); an empty allowlist exposes nothing.
perpsCandleRollupReadEnabled :: PerpsCandleReadMode -> Bool -> [Integer] -> Integer -> Bool
perpsCandleRollupReadEnabled mode strictCoverage allowlistedIntervals interval =
  mode == PerpsCandleReadsRollup
    && strictCoverage
    && interval `elem` canonicalBasketCandleIntervals
    && interval `elem` allowlistedIntervals

-- | A public rollup read requires the corresponding live writers. Without
-- this invariant, a reorg processed while writes are disabled could leave
-- previously complete rollups and coverage metadata available to the API.
validatePerpsCandleModeCombination
  :: PerpsCandleWriteMode
  -> PerpsCandleReadMode
  -> [Integer]
  -> Bool
  -> Either String ()
validatePerpsCandleModeCombination writeMode readMode readIntervals strictCoverage
  | not (null readIntervals)
      && writeMode /= PerpsCandleWritesDual =
      Left
        "PERPS_CANDLE_WRITE_MODE must be dual before any rollup interval is allowlisted"
  | readMode == PerpsCandleReadsRollup && not strictCoverage =
      Left
        "PERPS_CANDLE_STRICT_COVERAGE must be true when PERPS_CANDLE_READ_MODE is rollup"
  | otherwise = Right ()

parseBoundedWholeNumber :: String -> Int -> Int -> String -> Either String Int
parseBoundedWholeNumber name lower upper raw =
  case readMaybe normalized of
    Just value
      | show value == normalized
      , value >= lower
      , value <= upper -> Right value
    _ ->
      Left $
        name
          <> " must be a whole number between "
          <> show lower
          <> " and "
          <> show upper
  where
    normalized = T.unpack $ T.strip $ T.pack raw

parseNonNegativeInteger :: String -> String -> Either String Integer
parseNonNegativeInteger name raw =
  case readMaybe normalized of
    Just value
      | show value == normalized
      , value >= 0 -> Right value
    _ -> Left $ name <> " must be a non-negative whole number"
  where
    normalized = T.unpack $ T.strip $ T.pack raw

isCanonicalVaultAddress :: String -> Bool
isCanonicalVaultAddress raw =
  let address = T.strip $ T.pack raw
   in T.length address == 42
        && T.toLower (T.take 2 address) == "0x"
        && isValidAddress address

validAaDeploymentAddresses :: String -> String -> String -> String -> Bool
validAaDeploymentAddresses usdc router engine clearinghouse =
  and
    [ reviewed usdc "0x1647e41f49ed6d688936092b5a291c4b28106343"
    , reviewed router "0x97a901de2b267c307e264fd5f71403f8072f73e7"
    , reviewed engine "0x3dc9c0a1f9c745a4b08bd5c2e6c7ae613561c20d"
    , reviewed clearinghouse "0x2f98787f6dcc3b1f2e4a2afa5acf410159b9f211"
    ]
  where
    reviewed raw expected =
      let value = T.toLower $ T.strip $ T.pack raw
       in isValidAddress value && value == expected
