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
  , validateInsightsCompetitionActivation
  ) where

import Data.Aeson (FromJSON (..), Value (..), eitherDecodeFileStrict, withObject, (.:))
import Data.List (intercalate, nub, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Plether.Utils.Address (isValidAddress)
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , CompetitionRules (..)
  , competitionRulesForSlug
  , defaultCompetitionSlug
  , september2026CompetitionSlug
  )
import Plether.Insights.Registration.Config
  ( RegistrationConfig (..)
  , loadRegistrationConfig
  )
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
  , cfgPerpsCfdEngineLens :: Text
  , cfgPerpsCfdEngineSettlementSidecar :: Text
  , cfgPerpsMarginClearinghouse :: Text
  , cfgPerpsPletherOracle :: Text
  , cfgPerpsAccountLens :: Text
  , cfgPerpsIndexerStartBlock :: Integer
  , cfgInsightsCompetitionRules :: CompetitionRules
  , cfgInsightsCompetitionReleaseManifest :: CompetitionReleaseManifest
  , cfgRegistrationConfig :: Maybe RegistrationConfig
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

julyPerpsAccountLens, julyPerpsUsdc, julyPerpsOrderRouter, julyPerpsCfdEngine, julyPerpsCfdEngineLens, julyPerpsCfdEngineSettlementSidecar, julyPerpsMarginClearinghouse, julyPerpsPletherOracle :: String
julyPerpsAccountLens = "0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1"
julyPerpsUsdc = "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
julyPerpsOrderRouter = "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
julyPerpsCfdEngine = "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a"
julyPerpsCfdEngineLens = "0xa9aA4097874e9622eAABeE68f65Ff5e3757728C5"
julyPerpsCfdEngineSettlementSidecar = "0x0b652c4d4610234e221403076c116292f935b424"
julyPerpsMarginClearinghouse = "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e"
julyPerpsPletherOracle = "0xADfEd3bf768D810309B97b4dF9F9E77Eaa3a401c"

julyPerpsIndexerStartBlock :: String
julyPerpsIndexerStartBlock = "288439939"

-- | Resolve the immutable rule set. September may start in a registration-only
-- state without a release ID; supplying the release ID is the explicit bind
-- signal and then every address must identify one complete new release.
validateInsightsCompetitionActivation
  :: Text
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Either String CompetitionRules
validateInsightsCompetitionActivation slug maybeReleaseId maybeUsdc maybeRouter maybeCfdEngine maybeCfdEngineLens maybeSettlementSidecar maybeClearinghouse maybeOracle maybeLens maybeIndexerStartBlock = do
  rules <-
    maybe
      (Left $ "INSIGHTS_ACTIVE_COMPETITION_SLUG is unknown: " <> T.unpack slug)
      Right
      (competitionRulesForSlug slug)
  if crSlug rules /= september2026CompetitionSlug
    then Right rules
    else if isMissing maybeReleaseId
      then Right rules
    else
      let supplied =
            [ ("PERPS_USDC", maybeUsdc, julyPerpsUsdc)
            , ("PERPS_ORDER_ROUTER", maybeRouter, julyPerpsOrderRouter)
            , ("PERPS_CFD_ENGINE", maybeCfdEngine, julyPerpsCfdEngine)
            , ("PERPS_CFD_ENGINE_LENS", maybeCfdEngineLens, julyPerpsCfdEngineLens)
            , ("PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR", maybeSettlementSidecar, julyPerpsCfdEngineSettlementSidecar)
            , ("PERPS_MARGIN_CLEARINGHOUSE", maybeClearinghouse, julyPerpsMarginClearinghouse)
            , ("PERPS_PLETHER_ORACLE", maybeOracle, julyPerpsPletherOracle)
            , ("PERPS_ACCOUNT_LENS", maybeLens, julyPerpsAccountLens)
            ]
          julyReleaseAddresses =
            [ T.toLower $ T.pack julyAddress
            | (_, _, julyAddress) <- supplied
            ]
          configuredAddresses =
            [ T.toLower $ T.strip $ T.pack configured
            | (_, Just configured, _) <- supplied
            , not $ T.null $ T.strip $ T.pack configured
            ]
          missingOrInherited =
            [ name
            | (name, value, _) <- supplied
            , maybe True
                (\configured ->
                  let normalized = T.toLower $ T.strip $ T.pack configured
                   in T.null normalized || normalized `elem` julyReleaseAddresses)
                value
            ]
          invalid =
            [ name
            | (name, Just configured, _) <- supplied
            , not (T.null $ T.strip $ T.pack configured)
            , let normalized = T.toLower $ T.strip $ T.pack configured
            , not (isValidAddress normalized) || normalized == zeroAddress
            ]
          invalidIndexerStart =
            case fmap (T.unpack . T.strip . T.pack) maybeIndexerStartBlock of
              Just configured
                | configured /= julyPerpsIndexerStartBlock
                , Just parsed <- readMaybe configured
                , parsed > (0 :: Integer) -> []
              _ -> ["PERPS_INDEXER_START_BLOCK"]
          invalidReleaseId =
            [ "INSIGHTS_COMPETITION_RELEASE_ID"
            | fmap (T.strip . T.pack) maybeReleaseId /= Just september2026CompetitionSlug
            ]
          duplicateRoles =
            [ "PERPS release address roles (addresses must be pairwise distinct)"
            | length configuredAddresses /= length (nub configuredAddresses)
            ]
          invalidValues = invalidReleaseId <> missingOrInherited <> invalidIndexerStart <> duplicateRoles
       in if not $ null invalidValues
            then Left $
              "INSIGHTS_ACTIVE_COMPETITION_SLUG=testnet-trading-2026-09 requires explicit new-release values (not inherited July defaults) for: "
                <> intercalate ", " invalidValues
            else if not $ null invalid
              then Left $ "Invalid September competition deployment address in: " <> intercalate ", " invalid
              else Right rules
  where
    zeroAddress = "0x0000000000000000000000000000000000000000"
    isMissing = maybe True (T.null . T.strip . T.pack)

validateRegistrationConfig
  :: CompetitionRules
  -> Either String (Maybe RegistrationConfig)
  -> Either String (CompetitionRules, Maybe RegistrationConfig)
validateRegistrationConfig rules configured = do
  maybeRegistration <- configured
  case maybeRegistration of
    Nothing -> Right (rules, Nothing)
    Just registration
      | crRegistrationClosesAt rules == Nothing ->
          Left "INSIGHTS_REGISTRATION_ENABLED=true requires an active competition with registration rules"
      | rcXCallbackCompetitionSlug registration /= crSlug rules ->
          Left "X_OAUTH_CALLBACK_URL competition slug must match INSIGHTS_ACTIVE_COMPETITION_SLUG"
      | fmap T.toLower (crTargetXHandle rules) /= Just (T.toLower $ rcXTargetHandle registration) ->
          Left "X_TARGET_HANDLE must match the active competition rules"
      | fmap toInteger (crMinimumXAccountAgeDays rules) /= Just (rcMinimumXAccountAgeDays registration) ->
          Left "The configured X account age must match the active competition rules"
      | rcRulesVersion registration /= crRulesVersion rules ->
          Left "INSIGHTS_REGISTRATION_RULES_VERSION must match the active competition rules version"
      | otherwise -> Right (rules, Just registration)

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
  registrationConfig <- loadRegistrationConfig
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
      mPerpsAccountLens <- lookupEnv "PERPS_ACCOUNT_LENS"
      mPerpsUsdc <- lookupEnv "PERPS_USDC"
      mPerpsOrderRouter <- lookupEnv "PERPS_ORDER_ROUTER"
      mPerpsCfdEngine <- lookupEnv "PERPS_CFD_ENGINE"
      mPerpsCfdEngineLens <- lookupEnv "PERPS_CFD_ENGINE_LENS"
      mPerpsCfdEngineSettlementSidecar <- lookupEnv "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR"
      mPerpsMarginClearinghouse <- lookupEnv "PERPS_MARGIN_CLEARINGHOUSE"
      mPerpsPletherOracle <- lookupEnv "PERPS_PLETHER_ORACLE"
      mPerpsIndexerStartBlockStr <- lookupEnv "PERPS_INDEXER_START_BLOCK"
      mInsightsCompetitionSlug <- lookupEnv "INSIGHTS_ACTIVE_COMPETITION_SLUG"
      mInsightsCompetitionReleaseId <- lookupEnv "INSIGHTS_COMPETITION_RELEASE_ID"
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
          perpsAccountLens = fromMaybe julyPerpsAccountLens mPerpsAccountLens
          perpsUsdc = fromMaybe julyPerpsUsdc mPerpsUsdc
          perpsOrderRouter = fromMaybe julyPerpsOrderRouter mPerpsOrderRouter
          perpsCfdEngine = fromMaybe julyPerpsCfdEngine mPerpsCfdEngine
          perpsCfdEngineLens = fromMaybe julyPerpsCfdEngineLens mPerpsCfdEngineLens
          perpsCfdEngineSettlementSidecar =
            fromMaybe julyPerpsCfdEngineSettlementSidecar mPerpsCfdEngineSettlementSidecar
          perpsMarginClearinghouse = fromMaybe julyPerpsMarginClearinghouse mPerpsMarginClearinghouse
          perpsPletherOracle = fromMaybe julyPerpsPletherOracle mPerpsPletherOracle
          perpsIndexerStartBlockStr = fromMaybe julyPerpsIndexerStartBlock mPerpsIndexerStartBlockStr
          insightsCompetitionSlug =
            case T.strip . T.pack <$> mInsightsCompetitionSlug of
              Just configured | not (T.null configured) -> configured
              _ -> defaultCompetitionSlug
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
                      perpsMarginClearinghouse
                      || septemberAaReleaseAccepted ->
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

          competitionConfig = do
            rules <-
              validateInsightsCompetitionActivation
                insightsCompetitionSlug
                mInsightsCompetitionReleaseId
                mPerpsUsdc
                mPerpsOrderRouter
                mPerpsCfdEngine
                mPerpsCfdEngineLens
                mPerpsCfdEngineSettlementSidecar
                mPerpsMarginClearinghouse
                mPerpsPletherOracle
                mPerpsAccountLens
                mPerpsIndexerStartBlockStr
            if crSlug rules == september2026CompetitionSlug && perpsChainId /= 421614
              then Left "The September 2026 Insights competition is supported only on PERPS_CHAIN_ID=421614"
              else do
                validated@(_, maybeRegistration) <- validateRegistrationConfig rules registrationConfig
                case (maybeRegistration, nonBlankText mDatabaseUrl) of
                  (Just _, Nothing) ->
                    Left "INSIGHTS_REGISTRATION_PROVISIONED=true requires DATABASE_URL"
                  _ -> Right validated

          septemberAaReleaseAccepted =
            case competitionConfig of
              Right (rules, _) ->
                crSlug rules == september2026CompetitionSlug
                  && maybe False (not . T.null . T.strip . T.pack) mInsightsCompetitionReleaseId
                  && all (isValidAddress . T.pack)
                    [perpsUsdc, perpsOrderRouter, perpsCfdEngine, perpsMarginClearinghouse]
              Left _ -> False

      case (validatePythLatestMaxAgeSeconds pythLatestMaxAgeStr, aaConfig, candleConfig, competitionConfig) of
        (Left err, _, _, _) -> pure $ Left err
        (_, Left err, _, _) -> pure $ Left err
        (_, _, Left err, _) -> pure $ Left err
        (_, _, _, Left err) -> pure $ Left err
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
          , Right (insightsCompetitionRules, resolvedRegistrationConfig)
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
                , cfgPerpsCfdEngineLens = T.pack perpsCfdEngineLens
                , cfgPerpsCfdEngineSettlementSidecar = T.pack perpsCfdEngineSettlementSidecar
                , cfgPerpsMarginClearinghouse = T.pack perpsMarginClearinghouse
                , cfgPerpsPletherOracle = T.pack perpsPletherOracle
                , cfgPerpsAccountLens = T.pack perpsAccountLens
                , cfgPerpsIndexerStartBlock = perpsIndexerStartBlock
                , cfgInsightsCompetitionRules = insightsCompetitionRules
                , cfgInsightsCompetitionReleaseManifest =
                    let defaultReleaseId
                          | crSlug insightsCompetitionRules == september2026CompetitionSlug = "release-pending"
                          | otherwise = T.unpack $ crSlug insightsCompetitionRules
                     in CompetitionReleaseManifest
                      { crmReleaseId =
                          T.strip $ T.pack $ fromMaybe defaultReleaseId mInsightsCompetitionReleaseId
                      , crmChainId = perpsChainId
                      , crmUsdc = T.pack perpsUsdc
                      , crmOrderRouter = T.pack perpsOrderRouter
                      , crmMarginClearinghouse = T.pack perpsMarginClearinghouse
                      , crmAccountLens = T.pack perpsAccountLens
                      , crmCfdEngine = T.pack perpsCfdEngine
                      , crmCfdEngineLens = T.pack perpsCfdEngineLens
                      , crmSettlementSidecar = T.pack perpsCfdEngineSettlementSidecar
                      , crmPletherOracle = T.pack perpsPletherOracle
                      , crmIndexerStartBlock = perpsIndexerStartBlock
                      }
                , cfgRegistrationConfig = resolvedRegistrationConfig
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
