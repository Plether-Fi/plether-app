module Plether.Config
  ( Config (..)
  , AaConfig (..)
  , FaucetGuardConfig (..)
  , LpSettlementMode (..)
  , NativeAaConfig (..)
  , NativeAaSafetyInput (..)
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
  , validateKeeperPollSeconds
  , parsePerpsCandleReadIntervals
  , parsePerpsCandleReadMode
  , parsePerpsCandleWriteMode
  , parseLpSettlementMode
  , resolveLpSettlementMode
  , parseLpSettlementLimits
  , validateLpSettlementChainId
  , validateLpSettlementPrivateKeyConfig
  , lpSettlementModeText
  , perpsCandleRollupReadEnabled
  , validatePerpsCandleModeCombination
  , validateInsightsCompetitionActivation
  , validateFaucetGuardConfig
  , validateNativeAaPresence
  , validateNativeAaSafety
  , validAaOriginSecret
  , validAaDeploymentAddresses
  , normalizeExternalSecurityRpcUrl
  ) where

import qualified Plether.Perps.Manifest as Manifest
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeFileStrict, withObject, (.:))
import Data.Char (isHexDigit)
import Data.List (intercalate, nub, sortBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
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
import Plether.Perps.Release (validatePerpsV2ReleaseConfig)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Config = Config
  { cfgRpcUrl :: Text
  , cfgRpcAuthToken :: Maybe Text
  , cfgChainId :: Integer
  , cfgPort :: Int
  , cfgCorsOrigins :: [Text]
  , cfgDeployments :: [Deployment]
  , cfgDatabaseUrl :: Maybe Text
  , cfgIndexerStartBlock :: Integer
  , cfgPythBenchmarksUrl :: Text
  , cfgPythHistoryUrl :: Text
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
  , cfgPerpsRpcAuthToken :: Maybe Text
  , cfgPerpsChainId :: Integer
  , cfgPerpsUsdc :: Text
  , cfgPerpsOrderRouter :: Text
  , cfgPerpsOrderLifecycleBook :: Maybe Text
  , cfgPerpsCfdEngine :: Text
  , cfgPerpsCfdEngineLens :: Text
  , cfgPerpsCfdEngineSettlementSidecar :: Text
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
  , cfgVaultHistoryConfirmations :: Integer
  , cfgInsightsCompetitionRules :: CompetitionRules
  , cfgInsightsCompetitionReleaseManifest :: CompetitionReleaseManifest
  , cfgRegistrationConfig :: Maybe RegistrationConfig
  , cfgAaConfig :: Maybe AaConfig
  , cfgFaucetGuardConfig :: Maybe FaucetGuardConfig
  , cfgNativeAaConfig :: Maybe NativeAaConfig
  , cfgFaucetPrivateKey :: Maybe Text
  , cfgKeeperPrivateKey :: Maybe Text
  , cfgKeeperPollSeconds :: Int
  , cfgKeeperIdlePollSeconds :: Int
  , cfgKeeperMaxBatchSize :: Int
  , cfgKeeperConfirmations :: Int
  , cfgKeeperGasBufferBps :: Integer
  , cfgKeeperFeeBufferBps :: Integer
  , cfgLpSettlementMode :: LpSettlementMode
  , cfgLpSettlementPrivateKey :: Maybe Text
  , cfgLpSettlementSeniorVault :: Text
  , cfgLpSettlementJuniorVault :: Text
  , cfgLpSettlementPollSeconds :: Int
  , cfgLpSettlementMaxDrainTransactions :: Int
  , cfgLpSettlementPendingReplacementSeconds :: Int
  , cfgLpSettlementMaxReplacements :: Int
  , cfgLpSettlementMaxTxCostWei :: Integer
  }

instance Show Config where
  show cfg =
    "Config {cfgChainId = "
      <> show (cfgChainId cfg)
      <> ", cfgPort = "
      <> show (cfgPort cfg)
      <> ", cfgDatabaseConfigured = "
      <> show (isJust $ cfgDatabaseUrl cfg)
      <> ", cfgPythApiKeyConfigured = "
      <> show (isJust $ cfgPythApiKey cfg)
      <> ", cfgPerpsChainId = "
      <> show (cfgPerpsChainId cfg)
      <> ", cfgPerpsOrderRouter = "
      <> show (cfgPerpsOrderRouter cfg)
      <> ", cfgPerpsHousePool = "
      <> show (cfgPerpsHousePool cfg)
      <> ", cfgPerpsSettlementMonitorLens = "
      <> show (cfgPerpsSettlementMonitorLens cfg)
      <> ", cfgFaucetPrivateKeyConfigured = "
      <> show (isJust $ cfgFaucetPrivateKey cfg)
      <> ", cfgKeeperPrivateKeyConfigured = "
      <> show (isJust $ cfgKeeperPrivateKey cfg)
      <> ", cfgLpSettlementMode = "
      <> show (cfgLpSettlementMode cfg)
      <> ", cfgLpSettlementPrivateKeyConfigured = "
      <> show (isJust $ cfgLpSettlementPrivateKey cfg)
      <> ", cfgLpSettlementSeniorVault = "
      <> show (cfgLpSettlementSeniorVault cfg)
      <> ", cfgLpSettlementJuniorVault = "
      <> show (cfgLpSettlementJuniorVault cfg)
      <> ", cfgLpSettlementPollSeconds = "
      <> show (cfgLpSettlementPollSeconds cfg)
      <> ", cfgLpSettlementMaxDrainTransactions = "
      <> show (cfgLpSettlementMaxDrainTransactions cfg)
      <> ", cfgLpSettlementPendingReplacementSeconds = "
      <> show (cfgLpSettlementPendingReplacementSeconds cfg)
      <> ", cfgLpSettlementMaxReplacements = "
      <> show (cfgLpSettlementMaxReplacements cfg)
      <> ", cfgLpSettlementMaxTxCostWei = "
      <> show (cfgLpSettlementMaxTxCostWei cfg)
      <> "}"

data LpSettlementMode
  = LpSettlementOff
  | LpSettlementObserve
  | LpSettlementExecute
  deriving stock (Eq, Show)

data LpSettlementSettings = LpSettlementSettings
  { lpsMode :: LpSettlementMode
  , lpsPrivateKey :: Maybe Text
  , lpsSeniorVault :: Text
  , lpsJuniorVault :: Text
  , lpsPollSeconds :: Int
  , lpsMaxDrainTransactions :: Int
  , lpsPendingReplacementSeconds :: Int
  , lpsMaxReplacements :: Int
  , lpsMaxTxCostWei :: Integer
  }

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

data FaucetGuardConfig = FaucetGuardConfig
  { fgcProxyOriginToken :: Text
  , fgcClientRequestsPerHour :: Int
  , fgcGlobalRequestsPerHour :: Int
  }
  deriving stock (Eq)

instance Show FaucetGuardConfig where
  show cfg =
    "FaucetGuardConfig {fgcProxyOriginToken = <redacted>, fgcClientRequestsPerHour = "
      <> show (fgcClientRequestsPerHour cfg)
      <> ", fgcGlobalRequestsPerHour = "
      <> show (fgcGlobalRequestsPerHour cfg)
      <> "}"

-- | Configuration for the self-hosted ERC-4337 bundler and Plether
-- verifying-paymaster policy service.  This deliberately remains separate
-- from 'AaConfig' so the legacy Pimlico recovery route can be kept online
-- during a native sponsorship rollout without coupling either provider's
-- availability or credentials to the other.
data NativeAaConfig = NativeAaConfig
  { naaProxyOriginToken :: Text
  , naaAltoRpcUrl :: Text
  , naaSecurityRpcUrl :: Text
  , naaPaymasterAddress :: Text
  , naaPaymasterCodeHash :: Text
  , naaPolicyId :: Text
  , naaSignerAddress :: Text
  , naaKmsKeyId :: Text
  , naaAccountCodeHash :: Text
  , naaSponsorshipEnabled :: Bool
  , naaSubmissionEnabled :: Bool
  , naaIpRateLimitPerMinute :: Int
  , naaFinalRateLimitPerMinute :: Int
  , naaAccountRateLimitPerMinute :: Int
  , naaMaxRequestBytes :: Int
  , naaValiditySeconds :: Integer
  , naaVerificationGasLimit :: Integer
  , naaPostOpGasLimit :: Integer
  , naaMaxCostWei :: Integer
  , naaAccountOutstandingWei :: Integer
  , naaClientOutstandingWei :: Integer
  , naaGlobalOutstandingWei :: Integer
  , naaAccountHourlyWei :: Integer
  , naaGlobalHourlyWei :: Integer
  , naaGlobalDailyWei :: Integer
  , naaCanaryOwners :: [Text]
  , naaGlobalRolloutEnabled :: Bool
  }

-- | Security-sensitive values whose relationships must be validated as one
-- unit before native sponsorship can be enabled.  Keeping this validator pure
-- makes the environment loader and its regression tests share the exact same
-- fail-closed boundary.
data NativeAaSafetyInput = NativeAaSafetyInput
  { nasiOriginToken :: Text
  , nasiSponsorshipEnabled :: Bool
  , nasiSubmissionEnabled :: Bool
  , nasiGlobalRolloutEnabled :: Bool
  , nasiCanaryOwners :: [Text]
  , nasiValiditySeconds :: Integer
  , nasiPaymasterCodeHash :: Text
  , nasiPolicyId :: Text
  , nasiAccountCodeHash :: Text
  , nasiMaxCostWei :: Integer
  , nasiAccountOutstandingWei :: Integer
  , nasiClientOutstandingWei :: Integer
  , nasiGlobalOutstandingWei :: Integer
  , nasiAccountHourlyWei :: Integer
  , nasiGlobalHourlyWei :: Integer
  , nasiGlobalDailyWei :: Integer
  }

instance Show NativeAaConfig where
  show cfg =
    "NativeAaConfig {naaProxyOriginToken = <redacted>, naaAltoRpcUrl = "
      <> show (naaAltoRpcUrl cfg)
      <> ", naaSecurityRpcUrl = <redacted>"
      <> ", naaPaymasterAddress = "
      <> show (naaPaymasterAddress cfg)
      <> ", naaPaymasterCodeHash = "
      <> show (naaPaymasterCodeHash cfg)
      <> ", naaPolicyId = "
      <> show (naaPolicyId cfg)
      <> ", naaSignerAddress = "
      <> show (naaSignerAddress cfg)
      <> ", naaKmsKeyId = <redacted>, naaAccountCodeHash = "
      <> show (naaAccountCodeHash cfg)
      <> ", naaSponsorshipEnabled = "
      <> show (naaSponsorshipEnabled cfg)
      <> ", naaSubmissionEnabled = "
      <> show (naaSubmissionEnabled cfg)
      <> ", naaCanaryOwners = "
      <> show (naaCanaryOwners cfg)
      <> ", naaGlobalRolloutEnabled = "
      <> show (naaGlobalRolloutEnabled cfg)
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

validateKeeperPollSeconds :: String -> String -> Either String (Int, Int)
validateKeeperPollSeconds activeRaw idleRaw = do
  active <- parseBoundedWholeNumber "KEEPER_POLL_SECONDS" 1 3_600 activeRaw
  idle <- parseBoundedWholeNumber "KEEPER_IDLE_POLL_SECONDS" 1 3_600 idleRaw
  if idle < active
    then Left "KEEPER_IDLE_POLL_SECONDS must be greater than or equal to KEEPER_POLL_SECONDS"
    else Right (active, idle)

validateFaucetGuardConfig
  :: Maybe String
  -> Maybe String
  -> String
  -> String
  -> Either String (Maybe FaucetGuardConfig)
validateFaucetGuardConfig maybePrivateKey maybeOriginToken clientLimitRaw globalLimitRaw = do
  clientLimit <-
    parseBoundedWholeNumber
      "FAUCET_CLIENT_REQUESTS_PER_HOUR"
      1
      1_000_000
      clientLimitRaw
  globalLimit <-
    parseBoundedWholeNumber
      "FAUCET_GLOBAL_REQUESTS_PER_HOUR"
      1
      1_000_000
      globalLimitRaw
  if globalLimit < clientLimit
    then Left "FAUCET_GLOBAL_REQUESTS_PER_HOUR must be at least FAUCET_CLIENT_REQUESTS_PER_HOUR"
    else
      case (nonBlankText maybePrivateKey, nonBlankText maybeOriginToken) of
        (Just _, Nothing) ->
          Left "FAUCET_PROXY_ORIGIN_TOKEN is required when FAUCET_PRIVATE_KEY is configured"
        (_, Just token)
          | maybe False ((/= token) . T.pack) maybeOriginToken ->
              Left "FAUCET_PROXY_ORIGIN_TOKEN must not have leading or trailing whitespace"
          | T.length token < 32 ->
              Left "FAUCET_PROXY_ORIGIN_TOKEN must contain at least 32 characters"
          | otherwise ->
              Right $
                Just
                  FaucetGuardConfig
                    { fgcProxyOriginToken = token
                    , fgcClientRequestsPerHour = clientLimit
                    , fgcGlobalRequestsPerHour = globalLimit
                    }
        (Nothing, Nothing) -> Right Nothing

loadConfig :: IO (Either String Config)
loadConfig = do
  registrationConfig <- loadRegistrationConfig
  mRpcUrl <- firstEnv ["RPC_URL", "PERPS_RPC_URL"]
  case mRpcUrl of
    Nothing -> pure $ Left "RPC_URL or PERPS_RPC_URL environment variable not set"
    Just rpcUrl -> do
      mRpcAuthToken <- lookupEnv "RPC_AUTH_TOKEN"
      chainIdStr <- fromMaybe "11155111" <$> lookupEnv "CHAIN_ID"
      portStr <- fromMaybe "3001" <$> lookupEnv "PORT"
      corsStr <- fromMaybe "http://localhost:5173" <$> lookupEnv "CORS_ORIGINS"
      mDatabaseUrl <- lookupEnv "DATABASE_URL"
      indexerBlockStr <- fromMaybe "0" <$> lookupEnv "INDEXER_START_BLOCK"
      pythBenchmarksUrl <- fromMaybe "https://benchmarks.pyth.network" <$> lookupEnv "PYTH_BENCHMARKS_URL"
      pythHistoryUrl <- fromMaybe "https://pyth.dourolabs.app/v1" <$> lookupEnv "PYTH_HISTORY_URL"
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
      mPerpsRpcAuthToken <- lookupEnv "PERPS_RPC_AUTH_TOKEN"
      perpsChainIdStr <- fromMaybe "421614" <$> lookupEnv "PERPS_CHAIN_ID"
      mPerpsAccountLens <- lookupEnv "PERPS_ACCOUNT_LENS"
      mPerpsUsdc <- lookupEnv "PERPS_USDC"
      mPerpsOrderRouter <- lookupEnv "PERPS_ORDER_ROUTER"
      mPerpsOrderLifecycleBook <- nonBlankText <$> lookupEnv "PERPS_ORDER_LIFECYCLE_BOOK"
      mPerpsCfdEngine <- lookupEnv "PERPS_CFD_ENGINE"
      mPerpsCfdEngineLens <- lookupEnv "PERPS_CFD_ENGINE_LENS"
      mPerpsCfdEngineSettlementSidecar <- lookupEnv "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR"
      mPerpsMarginClearinghouse <- lookupEnv "PERPS_MARGIN_CLEARINGHOUSE"
      mPerpsPletherOracle <- lookupEnv "PERPS_PLETHER_ORACLE"
      mPerpsIndexerStartBlockStr <- lookupEnv "PERPS_INDEXER_START_BLOCK"
      perpsHousePool <- fromMaybe (T.unpack Manifest.housePoolAddress) <$> lookupEnv "PERPS_HOUSE_POOL"
      perpsSettlementMonitorLens <- fromMaybe (T.unpack Manifest.settlementMonitorLensAddress) <$> lookupEnv "PERPS_SETTLEMENT_MONITOR_LENS"
      vaultHistoryHousePool <- fromMaybe (T.unpack Manifest.housePoolAddress) <$> lookupEnv "VAULT_HISTORY_HOUSE_POOL_ADDRESS"
      vaultHistorySeniorVault <- fromMaybe (T.unpack Manifest.seniorVaultAddress) <$> lookupEnv "VAULT_HISTORY_SENIOR_VAULT_ADDRESS"
      vaultHistoryJuniorVault <- fromMaybe (T.unpack Manifest.juniorVaultAddress) <$> lookupEnv "VAULT_HISTORY_JUNIOR_VAULT_ADDRESS"
      vaultHistoryDeploymentBlockStr <- fromMaybe (show Manifest.releaseDeploymentBlock) <$> lookupEnv "VAULT_HISTORY_DEPLOYMENT_BLOCK"
      vaultHistoryConfirmationsStr <- fromMaybe "12" <$> lookupEnv "VAULT_HISTORY_CONFIRMATIONS"
      mInsightsCompetitionSlug <- lookupEnv "INSIGHTS_ACTIVE_COMPETITION_SLUG"
      mInsightsCompetitionReleaseId <- lookupEnv "INSIGHTS_COMPETITION_RELEASE_ID"
      mAaProxyOriginToken <- firstEnv ["AA_PROXY_ORIGIN_TOKEN"]
      mPimlicoApiKey <- firstEnv ["PIMLICO_API_KEY"]
      mPimlicoPolicyId <- firstEnv ["PIMLICO_SPONSORSHIP_POLICY_ID"]
      aaSponsorshipEnabledStr <- fromMaybe "false" <$> lookupEnv "AA_SPONSORSHIP_ENABLED"
      aaIpRateLimitStr <- fromMaybe "120" <$> lookupEnv "AA_IP_RATE_LIMIT_PER_MINUTE"
      aaAccountRateLimitStr <- fromMaybe "30" <$> lookupEnv "AA_ACCOUNT_RATE_LIMIT_PER_MINUTE"
      aaMaxRequestBytesStr <- fromMaybe "262144" <$> lookupEnv "AA_MAX_REQUEST_BYTES"
      aaSponsoredGasAlertWeiStr <- fromMaybe "0" <$> lookupEnv "AA_SPONSORED_GAS_ALERT_WEI_PER_HOUR"
      nativeAaEnabledStr <- fromMaybe "false" <$> lookupEnv "AA_NATIVE_SPONSORSHIP_ENABLED"
      nativeAaSubmissionEnabledStr <- fromMaybe "false" <$> lookupEnv "AA_NATIVE_SUBMISSION_ENABLED"
      nativeAaFinalRateLimitStr <- fromMaybe "6" <$> lookupEnv "AA_PAYMASTER_FINAL_RATE_LIMIT_PER_MINUTE"
      mAltoRpcUrl <- firstEnv ["AA_ALTO_RPC_URL"]
      mNativeSecurityRpcUrl <- firstEnv ["AA_RECONCILER_SECONDARY_RPC_URL"]
      mPaymasterAddress <- firstEnv ["AA_PAYMASTER_ADDRESS"]
      mPaymasterCodeHash <- firstEnv ["AA_PAYMASTER_CODE_HASH"]
      mNativePolicyId <- firstEnv ["AA_PAYMASTER_POLICY_ID"]
      mPaymasterSignerAddress <- firstEnv ["AA_PAYMASTER_SIGNER_ADDRESS"]
      mPaymasterKmsKeyId <- firstEnv ["AA_PAYMASTER_KMS_KEY_ID"]
      mPaymasterAccountCodeHash <- firstEnv ["AA_PAYMASTER_ACCOUNT_CODE_HASH"]
      paymasterValiditySecondsStr <- fromMaybe "300" <$> lookupEnv "AA_PAYMASTER_VALIDITY_SECONDS"
      paymasterVerificationGasLimitStr <- fromMaybe "100000" <$> lookupEnv "AA_PAYMASTER_VERIFICATION_GAS_LIMIT"
      paymasterPostOpGasLimitStr <- fromMaybe "0" <$> lookupEnv "AA_PAYMASTER_POST_OP_GAS_LIMIT"
      paymasterMaxCostWeiStr <- fromMaybe "10000000000000000" <$> lookupEnv "AA_PAYMASTER_MAX_COST_WEI"
      paymasterAccountOutstandingWeiStr <- fromMaybe "20000000000000000" <$> lookupEnv "AA_PAYMASTER_ACCOUNT_OUTSTANDING_WEI"
      paymasterClientOutstandingWeiStr <- fromMaybe "20000000000000000" <$> lookupEnv "AA_PAYMASTER_CLIENT_OUTSTANDING_WEI"
      paymasterGlobalOutstandingWeiStr <- fromMaybe "100000000000000000" <$> lookupEnv "AA_PAYMASTER_GLOBAL_OUTSTANDING_WEI"
      paymasterAccountHourlyWeiStr <- fromMaybe "30000000000000000" <$> lookupEnv "AA_PAYMASTER_ACCOUNT_HOURLY_WEI"
      paymasterGlobalHourlyWeiStr <- fromMaybe "100000000000000000" <$> lookupEnv "AA_PAYMASTER_GLOBAL_HOURLY_WEI"
      paymasterGlobalDailyWeiStr <- fromMaybe "250000000000000000" <$> lookupEnv "AA_PAYMASTER_GLOBAL_DAILY_WEI"
      nativeCanaryOwnersStr <- fromMaybe "" <$> lookupEnv "AA_NATIVE_CANARY_OWNERS"
      nativeGlobalRolloutEnabledStr <- fromMaybe "false" <$> lookupEnv "AA_NATIVE_GLOBAL_ROLLOUT_ENABLED"
      mFaucetPrivateKey <- lookupEnv "FAUCET_PRIVATE_KEY"
      mFaucetProxyOriginToken <- lookupEnv "FAUCET_PROXY_ORIGIN_TOKEN"
      faucetClientRequestsPerHourStr <- fromMaybe "20" <$> lookupEnv "FAUCET_CLIENT_REQUESTS_PER_HOUR"
      faucetGlobalRequestsPerHourStr <- fromMaybe "200" <$> lookupEnv "FAUCET_GLOBAL_REQUESTS_PER_HOUR"
      mKeeperPrivateKey <- lookupEnv "KEEPER_PRIVATE_KEY"
      keeperPollSecondsStr <- fromMaybe "1" <$> lookupEnv "KEEPER_POLL_SECONDS"
      keeperIdlePollSecondsStr <- fromMaybe "5" <$> lookupEnv "KEEPER_IDLE_POLL_SECONDS"
      keeperMaxBatchSizeStr <- fromMaybe "20" <$> lookupEnv "KEEPER_MAX_BATCH_SIZE"
      keeperConfirmationsStr <- fromMaybe "1" <$> lookupEnv "KEEPER_CONFIRMATIONS"
      keeperGasBufferBpsStr <- fromMaybe "2000" <$> lookupEnv "KEEPER_GAS_BUFFER_BPS"
      keeperFeeBufferBpsStr <- fromMaybe "2500" <$> lookupEnv "KEEPER_FEE_BUFFER_BPS"
      mLpSettlementMode <- lookupEnv "LP_SETTLEMENT_MODE"
      mLpSettlementEnabled <- lookupEnv "LP_SETTLEMENT_ENABLED"
      mLpSettlementPrivateKey <- lookupEnv "LP_SETTLEMENT_PRIVATE_KEY"
      lpSettlementSeniorVault <-
        fromMaybe (T.unpack Manifest.seniorVaultAddress)
          <$> lookupEnv "PERPS_SENIOR_VAULT"
      lpSettlementJuniorVault <-
        fromMaybe (T.unpack Manifest.juniorVaultAddress)
          <$> lookupEnv "PERPS_JUNIOR_VAULT"
      lpSettlementPollSecondsStr <- fromMaybe "15" <$> lookupEnv "LP_SETTLEMENT_POLL_SECONDS"
      lpSettlementMaxDrainTransactionsStr <-
        fromMaybe "4" <$> lookupEnv "LP_SETTLEMENT_MAX_DRAIN_TRANSACTIONS"
      lpSettlementPendingReplacementSecondsStr <-
        fromMaybe "60" <$> lookupEnv "LP_SETTLEMENT_PENDING_REPLACEMENT_SECONDS"
      lpSettlementMaxReplacementsStr <-
        fromMaybe "3" <$> lookupEnv "LP_SETTLEMENT_MAX_REPLACEMENTS"
      lpSettlementMaxTxCostWeiStr <-
        fromMaybe "0" <$> lookupEnv "LP_SETTLEMENT_MAX_TX_COST_WEI"

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
              (Just _, _, Nothing, Nothing) -> Right Nothing
              (Just aaSponsorshipEnabled, Just originToken, Just apiKey, Just policyId)
                | not (validAaOriginSecret $ T.pack originToken) ->
                    Left "AA_PROXY_ORIGIN_TOKEN must be a generated 64-character lowercase hex secret"
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
                | Left releaseFailure <-
                    validatePerpsV2ReleaseConfig
                      perpsChainId
                      (T.pack perpsOrderRouter)
                      mPerpsOrderLifecycleBook
                      (T.pack perpsCfdEngine)
                      (T.pack perpsMarginClearinghouse)
                      (T.pack perpsHousePool)
                      perpsIndexerStartBlock ->
                    Left $ T.unpack releaseFailure
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

          faucetGuardConfig =
            validateFaucetGuardConfig
              mFaucetPrivateKey
              mFaucetProxyOriginToken
              faucetClientRequestsPerHourStr
              faucetGlobalRequestsPerHourStr

          nativeAaConfig = do
            enabled <-
              maybe
                (Left "AA_NATIVE_SPONSORSHIP_ENABLED must be a boolean")
                Right
                (parseBoolStrict nativeAaEnabledStr)
            submissionEnabled <-
              maybe
                (Left "AA_NATIVE_SUBMISSION_ENABLED must be a boolean")
                Right
                (parseBoolStrict nativeAaSubmissionEnabledStr)
            globalRolloutEnabled <-
              maybe
                (Left "AA_NATIVE_GLOBAL_ROLLOUT_ENABLED must be a boolean")
                Right
                (parseBoolStrict nativeGlobalRolloutEnabledStr)
            let nativeSpecific =
                  [ mAltoRpcUrl
                  , mNativeSecurityRpcUrl
                  , mPaymasterAddress
                  , mPaymasterCodeHash
                  , mNativePolicyId
                  , mPaymasterSignerAddress
                  , mPaymasterKmsKeyId
                  , mPaymasterAccountCodeHash
                  ]
            nativeConfigured <-
              validateNativeAaPresence
                enabled
                submissionEnabled
                globalRolloutEnabled
                mAaProxyOriginToken
                nativeSpecific
            if not nativeConfigured
              then Right Nothing
              else case (mAaProxyOriginToken, nativeSpecific) of
                ( Just originToken
                  , [ Just altoRpcUrl
                  , Just securityRpcUrl
                  , Just paymasterAddress
                  , Just paymasterCodeHash
                  , Just policyId
                  , Just signerAddress
                  , Just kmsKeyId
                  , Just accountCodeHash
                  ]) -> do
                    validitySeconds <- parseDecimalBetween "AA_PAYMASTER_VALIDITY_SECONDS" 1 570 paymasterValiditySecondsStr
                    finalRateLimit <- parseDecimalBetween "AA_PAYMASTER_FINAL_RATE_LIMIT_PER_MINUTE" 1 60 nativeAaFinalRateLimitStr
                    verificationGasLimit <- parseDecimalBetween "AA_PAYMASTER_VERIFICATION_GAS_LIMIT" 1 (2 ^ (128 :: Integer) - 1) paymasterVerificationGasLimitStr
                    postOpGasLimit <- parseDecimalBetween "AA_PAYMASTER_POST_OP_GAS_LIMIT" 0 (2 ^ (128 :: Integer) - 1) paymasterPostOpGasLimitStr
                    maxCostWei <- parsePositiveDecimal "AA_PAYMASTER_MAX_COST_WEI" paymasterMaxCostWeiStr
                    accountOutstandingWei <- parsePositiveDecimal "AA_PAYMASTER_ACCOUNT_OUTSTANDING_WEI" paymasterAccountOutstandingWeiStr
                    clientOutstandingWei <- parsePositiveDecimal "AA_PAYMASTER_CLIENT_OUTSTANDING_WEI" paymasterClientOutstandingWeiStr
                    globalOutstandingWei <- parsePositiveDecimal "AA_PAYMASTER_GLOBAL_OUTSTANDING_WEI" paymasterGlobalOutstandingWeiStr
                    accountHourlyWei <- parsePositiveDecimal "AA_PAYMASTER_ACCOUNT_HOURLY_WEI" paymasterAccountHourlyWeiStr
                    globalHourlyWei <- parsePositiveDecimal "AA_PAYMASTER_GLOBAL_HOURLY_WEI" paymasterGlobalHourlyWeiStr
                    globalDailyWei <- parsePositiveDecimal "AA_PAYMASTER_GLOBAL_DAILY_WEI" paymasterGlobalDailyWeiStr
                    canaryOwners <- parseCanonicalAddressList "AA_NATIVE_CANARY_OWNERS" nativeCanaryOwnersStr
                    unlessEither
                      (perpsChainId == 421614)
                      "Native AA sponsorship is supported only on PERPS_CHAIN_ID=421614"
                    unlessEither
                      ( validAaDeploymentAddresses
                          perpsUsdc
                          perpsOrderRouter
                          perpsCfdEngine
                          perpsMarginClearinghouse
                      )
                      "Native AA sponsorship requires the reviewed Arbitrum Sepolia PERPS_USDC, PERPS_ORDER_ROUTER, PERPS_CFD_ENGINE, and PERPS_MARGIN_CLEARINGHOUSE deployment addresses"
                    unlessEither
                      (isJust mDatabaseUrl)
                      "DATABASE_URL is required whenever native AA is configured"
                    validateNativeAaSafety
                      NativeAaSafetyInput
                        { nasiOriginToken = T.pack originToken
                        , nasiSponsorshipEnabled = enabled
                        , nasiSubmissionEnabled = submissionEnabled
                        , nasiGlobalRolloutEnabled = globalRolloutEnabled
                        , nasiCanaryOwners = canaryOwners
                        , nasiValiditySeconds = validitySeconds
                        , nasiPaymasterCodeHash = T.pack paymasterCodeHash
                        , nasiPolicyId = T.pack policyId
                        , nasiAccountCodeHash = T.pack accountCodeHash
                        , nasiMaxCostWei = maxCostWei
                        , nasiAccountOutstandingWei = accountOutstandingWei
                        , nasiClientOutstandingWei = clientOutstandingWei
                        , nasiGlobalOutstandingWei = globalOutstandingWei
                        , nasiAccountHourlyWei = accountHourlyWei
                        , nasiGlobalHourlyWei = globalHourlyWei
                        , nasiGlobalDailyWei = globalDailyWei
                        }
                    unlessEither
                      (postOpGasLimit == 0)
                      "AA_PAYMASTER_POST_OP_GAS_LIMIT must be zero for the reviewed paymaster"
                    unlessEither
                      (validInternalRpcUrl $ T.pack altoRpcUrl)
                      "AA_ALTO_RPC_URL must be an http(s) URL without credentials or query parameters"
                    primarySecurityRpc <-
                      maybe
                        (Left "PERPS_RPC_URL must be a normalized HTTPS/default-443 URL without credentials, query, fragment, or whitespace when native AA is configured")
                        Right
                        (normalizeExternalSecurityRpcUrl $ T.pack perpsRpcUrl)
                    secondarySecurityRpc <-
                      maybe
                        (Left "AA_RECONCILER_SECONDARY_RPC_URL must be a normalized HTTPS/default-443 URL without credentials, query, fragment, or whitespace")
                        Right
                        (normalizeExternalSecurityRpcUrl $ T.pack securityRpcUrl)
                    unlessEither
                      (secondarySecurityRpc /= primarySecurityRpc)
                      "AA_RECONCILER_SECONDARY_RPC_URL must be independent from PERPS_RPC_URL"
                    unlessEither
                      (validNonzeroAddress $ T.pack paymasterAddress)
                      "AA_PAYMASTER_ADDRESS must be a nonzero 20-byte address"
                    unlessEither
                      (validNonzeroAddress $ T.pack signerAddress)
                      "AA_PAYMASTER_SIGNER_ADDRESS must be a nonzero 20-byte address"
                    unlessEither
                      (not $ T.null $ T.strip $ T.pack kmsKeyId)
                      "AA_PAYMASTER_KMS_KEY_ID must not be blank"
                    Right $
                      Just $
                        NativeAaConfig
                          { naaProxyOriginToken = T.pack originToken
                          , naaAltoRpcUrl = T.strip $ T.pack altoRpcUrl
                          , naaSecurityRpcUrl = secondarySecurityRpc
                          , naaPaymasterAddress = T.toLower $ T.strip $ T.pack paymasterAddress
                          , naaPaymasterCodeHash = T.toLower $ T.strip $ T.pack paymasterCodeHash
                          , naaPolicyId = T.toLower $ T.strip $ T.pack policyId
                          , naaSignerAddress = T.toLower $ T.strip $ T.pack signerAddress
                          , naaKmsKeyId = T.strip $ T.pack kmsKeyId
                          , naaAccountCodeHash = T.toLower $ T.strip $ T.pack accountCodeHash
                          , naaSponsorshipEnabled = enabled
                          , naaSubmissionEnabled = submissionEnabled
                          , naaIpRateLimitPerMinute = max 1 aaIpRateLimit
                          , naaFinalRateLimitPerMinute = fromInteger finalRateLimit
                          , naaAccountRateLimitPerMinute = max 1 aaAccountRateLimit
                          , naaMaxRequestBytes = max 1024 aaMaxRequestBytes
                          , naaValiditySeconds = validitySeconds
                          , naaVerificationGasLimit = verificationGasLimit
                          , naaPostOpGasLimit = postOpGasLimit
                          , naaMaxCostWei = maxCostWei
                          , naaAccountOutstandingWei = accountOutstandingWei
                          , naaClientOutstandingWei = clientOutstandingWei
                          , naaGlobalOutstandingWei = globalOutstandingWei
                          , naaAccountHourlyWei = accountHourlyWei
                          , naaGlobalHourlyWei = globalHourlyWei
                          , naaGlobalDailyWei = globalDailyWei
                          , naaCanaryOwners = canaryOwners
                          , naaGlobalRolloutEnabled = globalRolloutEnabled
                          }
                _ ->
                  Left
                    "Native AA configuration is partial; set AA_PROXY_ORIGIN_TOKEN, AA_ALTO_RPC_URL, AA_RECONCILER_SECONDARY_RPC_URL, \
                    \AA_PAYMASTER_ADDRESS, AA_PAYMASTER_CODE_HASH, AA_PAYMASTER_POLICY_ID, AA_PAYMASTER_SIGNER_ADDRESS, \
                    \AA_PAYMASTER_KMS_KEY_ID, and AA_PAYMASTER_ACCOUNT_CODE_HASH"

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
            if confirmations /= 12
              then Left "VAULT_HISTORY_CONFIRMATIONS must be exactly 12 for the reviewed Arbitrum Sepolia vault index"
              else Right ()
            let addresses =
                  [ ("VAULT_HISTORY_HOUSE_POOL_ADDRESS", vaultHistoryHousePool)
                  , ("VAULT_HISTORY_SENIOR_VAULT_ADDRESS", vaultHistorySeniorVault)
                  , ("VAULT_HISTORY_JUNIOR_VAULT_ADDRESS", vaultHistoryJuniorVault)
                  ]
            case [name | (name, address) <- addresses, not $ isCanonicalVaultAddress address] of
              invalid : _ -> Left $ invalid <> " must be a valid Ethereum address"
              [] ->
                Right (deploymentBlock, confirmations)

          lpSettlementConfig = do
            mode <- resolveLpSettlementMode mLpSettlementMode mLpSettlementEnabled
            validateLpSettlementChainId mode perpsChainIdStr
            pollSeconds <-
              parseBoundedWholeNumber
                "LP_SETTLEMENT_POLL_SECONDS"
                1
                3_600
                lpSettlementPollSecondsStr
            if mode /= LpSettlementOff && pollSeconds /= 15
              then Left "LP_SETTLEMENT_POLL_SECONDS must be exactly 15 in observe or execute mode"
              else Right ()
            if mode /= LpSettlementOff
              then do
                _ <-
                  parseBoundedWholeNumber
                    "KEEPER_CONFIRMATIONS"
                    1
                    10_000
                    keeperConfirmationsStr
                Right ()
              else Right ()
            (maxDrainTransactions, pendingReplacementSeconds, maxReplacements, maxTxCostWei) <-
              parseLpSettlementLimits
                mode
                lpSettlementMaxDrainTransactionsStr
                lpSettlementPendingReplacementSecondsStr
                lpSettlementMaxReplacementsStr
                lpSettlementMaxTxCostWeiStr
            privateKey <-
              validateLpSettlementPrivateKeyConfig
                mode
                (nonBlankText mLpSettlementPrivateKey)
                (nonBlankText mKeeperPrivateKey)
            let active = mode /= LpSettlementOff
                activeAddresses =
                  [ ("PERPS_HOUSE_POOL", perpsHousePool)
                  , ("PERPS_SETTLEMENT_MONITOR_LENS", perpsSettlementMonitorLens)
                  , ("PERPS_ORDER_ROUTER", perpsOrderRouter)
                  , ("PERPS_CFD_ENGINE", perpsCfdEngine)
                  , ("PERPS_SENIOR_VAULT", lpSettlementSeniorVault)
                  , ("PERPS_JUNIOR_VAULT", lpSettlementJuniorVault)
                  , ("PERPS_PLETHER_ORACLE", perpsPletherOracle)
                  ]
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
                    == (T.toLower Manifest.settlementMonitorLensSidecarAddress) ->
                    Left "PERPS_SETTLEMENT_MONITOR_LENS must be the facade, not the v1.2.1 monitor sidecar"
                | active
                , invalid : _ <-
                    [ name
                    | (name, address) <- activeAddresses
                    , not $ isNonZeroCanonicalVaultAddress address
                    ] ->
                    Left $ invalid <> " must be a valid non-zero Ethereum address"
                | active
                , length (nub $ map (T.toLower . T.strip . T.pack . snd) activeAddresses)
                    /= length activeAddresses ->
                    Left
                      "PERPS_HOUSE_POOL, PERPS_SETTLEMENT_MONITOR_LENS, PERPS_ORDER_ROUTER, \
                      \PERPS_CFD_ENGINE, PERPS_SENIOR_VAULT, PERPS_JUNIOR_VAULT, and \
                      \PERPS_PLETHER_ORACLE must be distinct"
                | otherwise ->
                    Right
                      LpSettlementSettings
                        { lpsMode = mode
                        , lpsPrivateKey = privateKey
                        , lpsSeniorVault = T.strip $ T.pack lpSettlementSeniorVault
                        , lpsJuniorVault = T.strip $ T.pack lpSettlementJuniorVault
                        , lpsPollSeconds = pollSeconds
                        , lpsMaxDrainTransactions = maxDrainTransactions
                        , lpsPendingReplacementSeconds = pendingReplacementSeconds
                        , lpsMaxReplacements = maxReplacements
                        , lpsMaxTxCostWei = maxTxCostWei
                        }

          keeperPollConfig =
            validateKeeperPollSeconds keeperPollSecondsStr keeperIdlePollSecondsStr

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

      case
          ( validatePythLatestMaxAgeSeconds pythLatestMaxAgeStr
          , aaConfig
          , nativeAaConfig
          , candleConfig
          , vaultHistoryConfig
          , lpSettlementConfig
          , keeperPollConfig
          , competitionConfig
          , faucetGuardConfig
          )
        of
        (Left err, _, _, _, _, _, _, _, _) -> pure $ Left err
        (_, Left err, _, _, _, _, _, _, _) -> pure $ Left err
        (_, _, Left err, _, _, _, _, _, _) -> pure $ Left err
        (_, _, _, Left err, _, _, _, _, _) -> pure $ Left err
        (_, _, _, _, Left err, _, _, _, _) -> pure $ Left err
        (_, _, _, _, _, Left err, _, _, _) -> pure $ Left err
        (_, _, _, _, _, _, Left err, _, _) -> pure $ Left err
        (_, _, _, _, _, _, _, Left err, _) -> pure $ Left err
        (_, _, _, _, _, _, _, _, Left err) -> pure $ Left err
        ( Right pythLatestMaxAgeSeconds
          , Right resolvedAaConfig
          , Right resolvedNativeAaConfig
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
                )
          , Right LpSettlementSettings {..}
          , Right (keeperPollSeconds, keeperIdlePollSeconds)
          , Right (insightsCompetitionRules, resolvedRegistrationConfig)
          , Right resolvedFaucetGuardConfig
          ) -> do
          eDeployments <- loadDeployments addressFile
          case eDeployments of
            Left err -> pure $ Left $ "Failed to load addresses: " <> err
            Right deployments ->
              pure $
                Right $
                  Config
                { cfgRpcUrl = T.pack rpcUrl
                , cfgRpcAuthToken = nonBlankText mRpcAuthToken
                , cfgChainId = chainId
                , cfgPort = port
                , cfgCorsOrigins = corsOrigins
                , cfgDeployments = deployments
                , cfgDatabaseUrl = fmap T.pack mDatabaseUrl
                , cfgIndexerStartBlock = indexerStartBlock
                , cfgPythBenchmarksUrl = T.pack pythBenchmarksUrl
                , cfgPythHistoryUrl = T.pack pythHistoryUrl
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
                , cfgPerpsRpcAuthToken = nonBlankText mPerpsRpcAuthToken
                , cfgPerpsChainId = perpsChainId
                , cfgPerpsUsdc = T.pack perpsUsdc
                , cfgPerpsOrderRouter = T.pack perpsOrderRouter
                , cfgPerpsOrderLifecycleBook = mPerpsOrderLifecycleBook
                , cfgPerpsCfdEngine = T.pack perpsCfdEngine
                , cfgPerpsCfdEngineLens = T.pack perpsCfdEngineLens
                , cfgPerpsCfdEngineSettlementSidecar = T.pack perpsCfdEngineSettlementSidecar
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
                , cfgVaultHistoryConfirmations = vaultHistoryConfirmations
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
                , cfgFaucetGuardConfig = resolvedFaucetGuardConfig
                , cfgNativeAaConfig = resolvedNativeAaConfig
                , cfgFaucetPrivateKey = nonBlankText mFaucetPrivateKey
                , cfgKeeperPrivateKey = fmap T.pack mKeeperPrivateKey
                , cfgKeeperPollSeconds = keeperPollSeconds
                , cfgKeeperIdlePollSeconds = keeperIdlePollSeconds
                , cfgKeeperMaxBatchSize = max 1 keeperMaxBatchSize
                , cfgKeeperConfirmations = max 0 keeperConfirmations
                , cfgKeeperGasBufferBps = max 0 keeperGasBufferBps
                , cfgKeeperFeeBufferBps = max 0 keeperFeeBufferBps
                , cfgLpSettlementMode = lpsMode
                , cfgLpSettlementPrivateKey = lpsPrivateKey
                , cfgLpSettlementSeniorVault = lpsSeniorVault
                , cfgLpSettlementJuniorVault = lpsJuniorVault
                , cfgLpSettlementPollSeconds = lpsPollSeconds
                , cfgLpSettlementMaxDrainTransactions = lpsMaxDrainTransactions
                , cfgLpSettlementPendingReplacementSeconds = lpsPendingReplacementSeconds
                , cfgLpSettlementMaxReplacements = lpsMaxReplacements
                , cfgLpSettlementMaxTxCostWei = lpsMaxTxCostWei
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

parseLpSettlementMode :: String -> Either String LpSettlementMode
parseLpSettlementMode raw =
  case T.toLower $ T.strip $ T.pack raw of
    "off" -> Right LpSettlementOff
    "observe" -> Right LpSettlementObserve
    "execute" -> Right LpSettlementExecute
    _ -> Left "LP_SETTLEMENT_MODE must be one of off, observe, or execute"

-- | Resolve the explicit rollout mode and the retired boolean flag without
-- ever interpreting the legacy switch as permission to execute transactions.
resolveLpSettlementMode :: Maybe String -> Maybe String -> Either String LpSettlementMode
resolveLpSettlementMode configuredMode legacyEnabled = do
  mode <- maybe (Right LpSettlementOff) parseLpSettlementMode configuredMode
  legacy <-
    case legacyEnabled of
      Nothing -> Right Nothing
      Just raw ->
        maybe
          (Left "LP_SETTLEMENT_ENABLED must be a boolean when present")
          (Right . Just)
          (parseBoolStrict raw)
  case legacy of
    Just True ->
      Left
        "LP_SETTLEMENT_ENABLED=true is no longer supported; remove it and set \
        \LP_SETTLEMENT_MODE explicitly"
    Just False
      | mode /= LpSettlementOff ->
          Left
            "LP_SETTLEMENT_ENABLED=false conflicts with an active LP_SETTLEMENT_MODE; \
            \remove the legacy variable before enabling settlement"
    _ -> Right mode

lpSettlementModeText :: LpSettlementMode -> Text
lpSettlementModeText = \case
  LpSettlementOff -> "off"
  LpSettlementObserve -> "observe"
  LpSettlementExecute -> "execute"

validateLpSettlementChainId :: LpSettlementMode -> String -> Either String ()
validateLpSettlementChainId mode rawChainId
  | mode == LpSettlementOff = Right ()
  | otherwise =
      case parseNonNegativeInteger "PERPS_CHAIN_ID" rawChainId of
        Right 421_614 -> Right ()
        _ ->
          Left
            "LP_SETTLEMENT_MODE observe and execute are supported only on \
            \PERPS_CHAIN_ID=421614"

parsePositiveDecimal :: String -> String -> Either String Integer
parsePositiveDecimal name = parseDecimalBetween name 1 (2 ^ (128 :: Integer) - 1)

parseDecimalBetween :: String -> Integer -> Integer -> String -> Either String Integer
parseDecimalBetween name minimum maximum raw =
  let normalized = T.unpack $ T.strip $ T.pack raw
   in case readMaybe normalized of
        Just value
          | show value == normalized
          , value >= minimum
          , value <= maximum -> Right value
        _ ->
          Left $
            name
              <> " must be a canonical decimal integer between "
              <> show minimum
              <> " and "
              <> show maximum

unlessEither :: Bool -> String -> Either String ()
unlessEither condition message =
  if condition then Right () else Left message

validFixedHexBytes :: Int -> Text -> Bool
validFixedHexBytes bytes raw =
  let value = T.toLower $ T.strip raw
      body = T.drop 2 value
   in T.isPrefixOf "0x" value
        && T.length body == bytes * 2
        && T.all isHexCharacter body

validNonzeroAddress :: Text -> Bool
validNonzeroAddress raw =
  validFixedHexBytes 20 raw
    && T.toLower (T.strip raw) /= "0x0000000000000000000000000000000000000000"

validInternalRpcUrl :: Text -> Bool
validInternalRpcUrl raw =
  let value = raw
      authority
        | "http://" `T.isPrefixOf` value = T.takeWhile (/= '/') $ T.drop 7 value
        | "https://" `T.isPrefixOf` value = T.takeWhile (/= '/') $ T.drop 8 value
        | otherwise = ""
      validAuthorityChar char =
        isHexCharacter char
          || (char >= 'g' && char <= 'z')
          || (char >= 'G' && char <= 'Z')
          || char `elem` (".-:[]" :: String)
   in raw == T.strip raw
        && not (T.any (`elem` [' ', '\t', '\r', '\n', '\0']) value)
        && not (T.null authority)
        && T.length authority <= 255
        && T.all validAuthorityChar authority
        && T.head authority /= '.'
        && T.last authority /= '.'
        && not ("@" `T.isInfixOf` value)
        && not ("?" `T.isInfixOf` value)
        && not ("#" `T.isInfixOf` value)

-- | Canonicalize the two security-critical external providers.  Requiring the
-- default HTTPS port removes spelling aliases that could accidentally defeat
-- the distinct-provider guard. DNS/provider independence remains an explicit
-- rollout attestation because it cannot be proven from a URL alone.
normalizeExternalSecurityRpcUrl :: Text -> Maybe Text
normalizeExternalSecurityRpcUrl raw = do
  let prefix = "https://"
      value = raw
  if raw /= T.strip raw || not (prefix `T.isPrefixOf` value)
    then Nothing
    else do
      let remainder = T.drop (T.length prefix) value
          (authority, path) = T.breakOn "/" remainder
          host = fromMaybe authority $ T.stripSuffix ":443" authority
          normalizedPath
            | T.null path || path == "/" = ""
            | otherwise = T.dropWhileEnd (== '/') path
          validHostChar char =
            (char >= 'a' && char <= 'z')
              || (char >= 'A' && char <= 'Z')
              || (char >= '0' && char <= '9')
              || char `elem` (".-" :: String)
      if T.null host
          || T.length host > 253
          || T.head host == '.'
          || T.last host == '.'
          || T.any (not . validHostChar) host
          || "@" `T.isInfixOf` value
          || "?" `T.isInfixOf` value
          || "#" `T.isInfixOf` value
          || T.any (`elem` [' ', '\t', '\r', '\n', '\0']) value
          || (":" `T.isInfixOf` authority && not (":443" `T.isSuffixOf` authority))
        then Nothing
        else Just $ prefix <> T.toLower host <> normalizedPath

reviewedNativePolicyId :: Text
reviewedNativePolicyId =
  "0x8dd77324b94da492342191f762a32cdf99e828a7f24d77c8ed5ace90cf4f5ae3"

reviewedNativeAccountCodeHash :: Text
reviewedNativeAccountCodeHash =
  "0x41ee894da413cc99e8dec0a1784470eceb736845ad1591e06ff0ecdf0aca26c9"

zeroNativeHash :: Text
zeroNativeHash = "0x" <> T.replicate 64 "0"

-- | Resolve whether the native configuration is absent or complete.  The
-- global rollout switch is intentionally unavailable until a separate release
-- explicitly enables it, so setting it is always an error (even when the rest
-- of the native configuration is absent).
validateNativeAaPresence
  :: Bool
  -> Bool
  -> Bool
  -> Maybe String
  -> [Maybe String]
  -> Either String Bool
validateNativeAaPresence enabled submissionEnabled globalRolloutEnabled originToken nativeSpecific
  | globalRolloutEnabled =
      Left
        "AA_NATIVE_GLOBAL_ROLLOUT_ENABLED=true is not supported; native sponsorship must remain canary-scoped"
  | maybe False (not . validAaOriginSecret . T.pack) originToken =
      Left "AA_PROXY_ORIGIN_TOKEN must be a generated 64-character lowercase hex secret"
  | all (== Nothing) nativeSpecific
      && not enabled
      && not submissionEnabled =
      Right False
  | isJust originToken && all isJust nativeSpecific = Right True
  | otherwise =
      Left
        "Native AA configuration is partial; set AA_PROXY_ORIGIN_TOKEN, AA_ALTO_RPC_URL, AA_RECONCILER_SECONDARY_RPC_URL, \
        \AA_PAYMASTER_ADDRESS, AA_PAYMASTER_CODE_HASH, AA_PAYMASTER_POLICY_ID, AA_PAYMASTER_SIGNER_ADDRESS, \
        \AA_PAYMASTER_KMS_KEY_ID, and AA_PAYMASTER_ACCOUNT_CODE_HASH"

-- | Validate the security relationships between individually parsed native AA
-- settings.  Callers must run this before constructing 'NativeAaConfig'.
validateNativeAaSafety :: NativeAaSafetyInput -> Either String ()
validateNativeAaSafety NativeAaSafetyInput{..} = do
  unlessEither
    (validAaOriginSecret nasiOriginToken)
    "AA_PROXY_ORIGIN_TOKEN must be a generated 64-character lowercase hex secret"
  unlessEither
    (not nasiGlobalRolloutEnabled)
    "AA_NATIVE_GLOBAL_ROLLOUT_ENABLED=true is not supported; native sponsorship must remain canary-scoped"
  unlessEither
    (not nasiSponsorshipEnabled || nasiSubmissionEnabled)
    "AA_NATIVE_SPONSORSHIP_ENABLED=true requires AA_NATIVE_SUBMISSION_ENABLED=true"
  unlessEither
    (not nasiSponsorshipEnabled || not (null nasiCanaryOwners))
    "Native sponsorship requires at least one AA_NATIVE_CANARY_OWNERS entry"
  unlessEither
    (nasiValiditySeconds >= 1 && nasiValiditySeconds <= 570)
    "AA_PAYMASTER_VALIDITY_SECONDS must be between 1 and 570"
  unlessEither
    ( validFixedHexBytes 32 nasiPaymasterCodeHash
        && T.toLower (T.strip nasiPaymasterCodeHash) /= zeroNativeHash
    )
    "AA_PAYMASTER_CODE_HASH must be a nonzero 32-byte runtime hash"
  unlessEither
    (T.toLower (T.strip nasiPolicyId) == reviewedNativePolicyId)
    "AA_PAYMASTER_POLICY_ID does not match the reviewed Plether paymaster policy"
  unlessEither
    (T.toLower (T.strip nasiAccountCodeHash) == reviewedNativeAccountCodeHash)
    "AA_PAYMASTER_ACCOUNT_CODE_HASH does not match the reviewed SimpleAccount proxy runtime"
  unlessEither
    ( nasiMaxCostWei <= nasiAccountOutstandingWei
        && nasiMaxCostWei <= nasiClientOutstandingWei
        && nasiAccountOutstandingWei <= nasiGlobalOutstandingWei
        && nasiClientOutstandingWei <= nasiGlobalOutstandingWei
    )
    "AA paymaster outstanding budgets must satisfy per-operation <= account/client <= global"
  unlessEither
    (nasiAccountHourlyWei <= nasiGlobalHourlyWei && nasiGlobalHourlyWei <= nasiGlobalDailyWei)
    "AA paymaster spend budgets must satisfy account-hourly <= global-hourly <= global-daily"

validAaOriginSecret :: Text -> Bool
validAaOriginSecret raw =
  T.length raw == 64
    && raw == T.strip raw
    && T.all (\char -> (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f')) raw
    && raw `notElem` knownAaOriginPlaceholders

knownAaOriginPlaceholders :: [Text]
knownAaOriginPlaceholders =
  [ T.replicate 64 "0"
  , T.replicate 64 "f"
  , T.concat $ replicate 4 "0123456789abcdef"
  , T.concat $ replicate 8 "deadbeef"
  ]

isHexCharacter :: Char -> Bool
isHexCharacter char =
  (char >= '0' && char <= '9')
    || (char >= 'a' && char <= 'f')
    || (char >= 'A' && char <= 'F')

parseCanonicalAddressList :: String -> String -> Either String [Text]
parseCanonicalAddressList name raw = do
  let tokens =
        filter (not . T.null) $
          concatMap T.words $
            T.splitOn "," $
              T.strip $
                T.pack raw
      normalized = map T.toLower tokens
  unlessEither
    (all validNonzeroAddress normalized)
    (name <> " may contain only nonzero 20-byte addresses separated by spaces or commas")
  unlessEither
    (Set.size (Set.fromList normalized) == length normalized)
    (name <> " must not contain duplicate addresses")
  Right normalized

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

parseLpSettlementLimits
  :: LpSettlementMode
  -> String
  -> String
  -> String
  -> String
  -> Either String (Int, Int, Int, Integer)
parseLpSettlementLimits mode maxDrainRaw replacementSecondsRaw maxReplacementsRaw maxTxCostRaw = do
  maxDrainTransactions <-
    parseBoundedWholeNumber
      "LP_SETTLEMENT_MAX_DRAIN_TRANSACTIONS"
      1
      4
      maxDrainRaw
  pendingReplacementSeconds <-
    parseBoundedWholeNumber
      "LP_SETTLEMENT_PENDING_REPLACEMENT_SECONDS"
      60
      3_600
      replacementSecondsRaw
  maxReplacements <-
    parseBoundedWholeNumber
      "LP_SETTLEMENT_MAX_REPLACEMENTS"
      0
      3
      maxReplacementsRaw
  maxTxCostWei <-
    parseNonNegativeInteger
      "LP_SETTLEMENT_MAX_TX_COST_WEI"
      maxTxCostRaw
  if mode == LpSettlementExecute && maxTxCostWei == 0
    then Left "LP_SETTLEMENT_MAX_TX_COST_WEI must be positive in execute mode"
    else
      Right
        ( maxDrainTransactions
        , pendingReplacementSeconds
        , maxReplacements
        , maxTxCostWei
        )

-- | Validate the environment-level key contract. The keeper executable also
-- derives the address before starting an active worker, which provides the
-- final secp256k1 scalar validity check.
validateLpSettlementPrivateKeyConfig
  :: LpSettlementMode
  -> Maybe Text
  -> Maybe Text
  -> Either String (Maybe Text)
validateLpSettlementPrivateKeyConfig mode privateKey keeperPrivateKey
  | Just key <- privateKey
  , not $ isValidPrivateKeyShape key =
      Left "LP_SETTLEMENT_PRIVATE_KEY must be a non-zero 32-byte hexadecimal private key"
  | Just lpKey <- privateKey
  , Just keeperKey <- keeperPrivateKey
  , normalizePrivateKeyText lpKey == normalizePrivateKeyText keeperKey =
      Left "LP_SETTLEMENT_PRIVATE_KEY must be distinct from KEEPER_PRIVATE_KEY"
  | mode /= LpSettlementOff && privateKey == Nothing =
      Left "LP_SETTLEMENT_PRIVATE_KEY is required when LP_SETTLEMENT_MODE is observe or execute"
  | otherwise = Right privateKey

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

isNonZeroCanonicalVaultAddress :: String -> Bool
isNonZeroCanonicalVaultAddress raw =
  isCanonicalVaultAddress raw
    && T.toLower (T.strip $ T.pack raw)
      /= "0x0000000000000000000000000000000000000000"

normalizePrivateKeyText :: Text -> Text
normalizePrivateKeyText value =
  T.toLower $ fromMaybe stripped $ T.stripPrefix "0x" stripped
  where
    stripped = T.strip value

isValidPrivateKeyShape :: Text -> Bool
isValidPrivateKeyShape value =
  T.length normalized == 64
    && T.all isHexDigit normalized
    && T.any (/= '0') normalized
  where
    normalized = normalizePrivateKeyText value

validAaDeploymentAddresses :: String -> String -> String -> String -> Bool
validAaDeploymentAddresses usdc router engine clearinghouse =
  and
    [ reviewed usdc (T.toLower Manifest.mockUsdcAddress)
    , reviewed router (T.toLower Manifest.orderRouterAddress)
    , reviewed engine (T.toLower Manifest.cfdEngineAddress)
    , reviewed clearinghouse (T.toLower Manifest.marginClearinghouseAddress)
    ]
  where
    reviewed raw expected =
      let value = T.toLower $ T.strip $ T.pack raw
       in isValidAddress value && value == expected
