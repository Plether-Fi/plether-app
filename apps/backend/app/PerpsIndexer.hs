module Main (main) where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.Config (Config (..), PerpsCandleWriteMode (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Insights (validateCompetitionReleaseManifest)
import Plether.Database.Protocol (ensureProtocolSchema)
import Plether.Database.Schema (ensurePerpsHistorySchema)
import Plether.Ethereum.Client
  ( RpcChainBindingError (..)
  , RpcError
  , ethChainId
  , newClientWithManager
  , selectRpcUrlsForChain
  , validateRpcChainId
  )
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , CompetitionRules (..)
  , competitionReleaseIsBound
  )
import Plether.Logging (field, logError, logInfo)
import Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , PerpsIndexerConfig (..)
  , PerpsIndexerMode (..)
  , defaultPerpsAddresses
  , perpsIndexerName
  , runPerpsIndexer
  )
import qualified Plether.Perps.IndexerOptions as IndexerOptions
import Plether.Protocol.Release (ProtocolRelease (..), currentProtocolRelease)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

data WorkerArgs = WorkerArgs
  { waMode :: PerpsIndexerMode
  , waConfirmations :: Integer
  , waBatchSize :: Integer
  , waPollSeconds :: Int
  , waStartBlock :: Maybe Integer
  , waRpcUrls :: Maybe [Text]
  , waTraceApiUrl :: Maybe Text
  , waAddresses :: PerpsAddresses
  }
  deriving (Show)

defaultConfirmations :: Integer
defaultConfirmations = 1

defaultBatchSize :: Integer
defaultBatchSize = 5000

defaultPollSeconds :: Int
defaultPollSeconds = 12

main :: IO ()
main = do
  cliArgs <- getArgs
  case IndexerOptions.parsePerpsIndexerInvocation cliArgs of
    Left err -> do
      logError
        "perps_indexer_cli_invalid"
        "Perps indexer command-line arguments are invalid"
        [field "error" err]
      exitFailure
    Right invocation -> do
      envArgs <- loadEnvArgs
      deploymentEnvironment <- fmap T.pack <$> lookupEnv "DEPLOYMENT_ENVIRONMENT"
      eConfig <- loadConfig
      case eConfig of
        Left err -> do
          logError
            "perps_indexer_configuration_invalid"
            "Perps indexer configuration is invalid"
            [field "error" err]
          exitFailure
        Right cfg ->
          case
            validateReplayDeployment
              invocation
              deploymentEnvironment
              (cfgPerpsCandleWriteMode cfg)
              envArgs
          of
            Left err -> do
              logError
                "perps_indexer_replay_configuration_invalid"
                "Bounded Perps replay configuration is invalid"
                [field "error" err]
              exitFailure
            Right () ->
              runConfiguredIndexer
                invocation
                deploymentEnvironment
                envArgs
                cliArgs
                cfg

runConfiguredIndexer
  :: IndexerOptions.PerpsIndexerInvocation
  -> Maybe Text
  -> [(String, String)]
  -> [String]
  -> Config
  -> IO ()
runConfiguredIndexer invocation deploymentEnvironment envArgs cliArgs cfg =
  let release = currentProtocolRelease cfg
      configuredAddresses =
        defaultPerpsAddresses
          { paUsdc = prUsdc release
          , paOrderRouter = prOrderRouter release
          , paOrderRouterAdmin = prOrderRouterAdmin release
          , paCfdEngine = prCfdEngine release
          , paCfdEngineAdmin = prCfdEngineAdmin release
          , paCfdEngineLens = cfgPerpsCfdEngineLens cfg
          , paCfdEngineSettlementSidecar =
              cfgPerpsCfdEngineSettlementSidecar cfg
          , paMarginClearinghouse = prMarginClearinghouse release
          , paPletherOracle = prPletherOracle release
          , paAccountLens = prAccountLens release
          , paPublicLens = prPublicLens release
          , paHousePool = prHousePool release
          , paSeniorVault = prSeniorVault release
          , paJuniorVault = prJuniorVault release
          }
      parsedArgs = parseWorkerArgs configuredAddresses envArgs cliArgs
      args =
        parsedArgs
          { waMode =
              case invocation of
                IndexerOptions.PerpsIndexerLoop -> waMode parsedArgs
                IndexerOptions.PerpsIndexerReplay replayOptions ->
                  PerpsIndexerReplay replayOptions
          }
   in case cfgDatabaseUrl cfg of
        Nothing -> do
          logError
            "perps_indexer_database_missing"
            "Perps indexer requires a database"
            []
          exitFailure
        Just dbUrl -> do
          manager <- newManager tlsManagerSettings
          let configuredRpcUrls =
                fromMaybe [cfgPerpsRpcUrl cfg] (waRpcUrls args)
          probeResults <- forM configuredRpcUrls $ \rpcUrl -> do
            client <- newClientWithManager manager rpcUrl
            observedChainId <- ethChainId client
            pure (rpcUrl, observedChainId)
          let matchingRpcUrls =
                selectRpcUrlsForChain (prChainId release) probeResults
              mismatchCount =
                countBindingFailures
                  (prChainId release)
                  RpcChainIdMismatch
                  probeResults
              unavailableCount =
                countBindingFailures
                  (prChainId release)
                  RpcChainIdUnavailable
                  probeResults
          if null matchingRpcUrls
            then do
              logError
                "perps_indexer_rpc_chain_binding_failed"
                "No configured RPC provider matches the protocol release chain"
                [ field "configured_provider_count" $ length configuredRpcUrls
                , field "mismatched_provider_count" mismatchCount
                , field "unavailable_provider_count" unavailableCount
                , field "expected_chain_id" $ prChainId release
                ]
              exitFailure
            else
              startValidatedIndexer
                manager
                dbUrl
                cfg
                release
                invocation
                deploymentEnvironment
                args
                matchingRpcUrls

startValidatedIndexer
  :: Manager
  -> Text
  -> Config
  -> ProtocolRelease
  -> IndexerOptions.PerpsIndexerInvocation
  -> Maybe Text
  -> WorkerArgs
  -> [Text]
  -> IO ()
startValidatedIndexer manager dbUrl cfg release invocation deploymentEnvironment args rpcUrls = do
  pool <- newDbPool dbUrl
  case invocation of
    IndexerOptions.PerpsIndexerLoop -> do
      withDb pool ensurePerpsHistorySchema
      withDb pool $ \conn -> ensureProtocolSchema conn release
    -- Replay is a duplicate-ingestion proof over a pre-existing schema. It may
    -- not run migrations or bootstrap a protocol release as a side effect.
    IndexerOptions.PerpsIndexerReplay _ -> pure ()
  let startBlock = prDeploymentBlock release
      requestedStartBlock =
        fromMaybe (cfgPerpsIndexerStartBlock cfg) (waStartBlock args)
      addresses = waAddresses args
      releaseManifest =
        (cfgInsightsCompetitionReleaseManifest cfg)
          { crmChainId = prChainId release
          , crmUsdc = paUsdc addresses
          , crmOrderRouter = paOrderRouter addresses
          , crmMarginClearinghouse = paMarginClearinghouse addresses
          , crmAccountLens = paAccountLens addresses
          , crmCfdEngine = paCfdEngine addresses
          , crmCfdEngineLens = paCfdEngineLens addresses
          , crmSettlementSidecar = paCfdEngineSettlementSidecar addresses
          , crmPletherOracle = paPletherOracle addresses
          , crmIndexerStartBlock = startBlock
          }
      traceApiUrl =
        case waTraceApiUrl args of
          Just value
            | T.null (T.strip value) -> Nothing
            | otherwise -> Just $ T.strip value
          Nothing
            | prChainId release == 421614 ->
                Just "https://arbitrum-sepolia.blockscout.com/api/v2"
            | otherwise -> Nothing
      indexerCfg =
        PerpsIndexerConfig
          { picRpcUrls = rpcUrls
          , picTraceApiUrl = traceApiUrl
          , picChainId = prChainId release
          , picReleaseId = prId release
          , picCalculationVersion = prCalculationVersion release
          , picAddresses = addresses
          , picStartBlock = startBlock
          , picConfirmations = waConfirmations args
          , picBatchSize = waBatchSize args
          , picPollIntervalMicros =
              max 1 (waPollSeconds args) * 1_000_000
          , picIndexerName = perpsIndexerName
          , picMode = waMode args
          , picCandleWriteMode = cfgPerpsCandleWriteMode cfg
          , picCandleLatenessSeconds = cfgPerpsCandleLatenessSeconds cfg
          , picDeploymentEnvironment = deploymentEnvironment
          }
  whenReleaseBound cfg releaseManifest $ \boundManifest ->
    withDb pool $ \conn ->
      validateCompetitionReleaseManifest
        conn
        (crSlug $ cfgInsightsCompetitionRules cfg)
        boundManifest
  logInfo
    "perps_indexer_started"
    "Perps history indexer started with release-bound RPC providers"
    [ field "mode" $ show $ waMode args
    , field "release_id" $ prId release
    , field "start_block" startBlock
    , field "requested_start_block" requestedStartBlock
    , field "confirmations" $ waConfirmations args
    , field "batch_size" $ waBatchSize args
    , field "poll_seconds" $ waPollSeconds args
    , field "rpc_provider_count" $ length rpcUrls
    , field "trace_api_fallback_enabled" $ maybe False (const True) traceApiUrl
    ]
  runPerpsIndexer manager pool indexerCfg

whenReleaseBound
  :: Config
  -> CompetitionReleaseManifest
  -> (CompetitionReleaseManifest -> IO ())
  -> IO ()
whenReleaseBound cfg manifest action
  | competitionReleaseIsBound (cfgInsightsCompetitionRules cfg) manifest =
      action manifest
  | otherwise = pure ()

countBindingFailures
  :: Integer
  -> RpcChainBindingError
  -> [(Text, Either RpcError Integer)]
  -> Int
countBindingFailures expectedChainId expectedFailure =
  length
    . filter
      ( \(_, observedChainId) ->
          validateRpcChainId expectedChainId observedChainId
            == Left expectedFailure
      )

validateReplayDeployment
  :: IndexerOptions.PerpsIndexerInvocation
  -> Maybe Text
  -> PerpsCandleWriteMode
  -> [(String, String)]
  -> Either Text ()
validateReplayDeployment IndexerOptions.PerpsIndexerLoop _ _ _ = Right ()
validateReplayDeployment (IndexerOptions.PerpsIndexerReplay _) deploymentEnvironment candleWriteMode env
  | deploymentEnvironment /= Just "sepolia" =
      Left "Bounded Perps replay is restricted to DEPLOYMENT_ENVIRONMENT=sepolia"
  | candleWriteMode /= PerpsCandleWritesDual
      || lookup "PERPS_CANDLE_WRITE_MODE" env /= Just "dual" =
      Left "Bounded Perps replay requires PERPS_CANDLE_WRITE_MODE=dual"
  | otherwise = do
      requireUnsignedEnv "PERPS_CHAIN_ID" 1 9_223_372_036_854_775_807 env
      requireUnsignedEnv "PERPS_INDEXER_START_BLOCK" 0 9_223_372_036_854_775_807 env
      requireUnsignedEnv "PERPS_INDEXER_CONFIRMATIONS" 0 10_000 env
      requireOptionalUnsignedEnv "PERPS_INDEXER_BATCH_SIZE" 1 5_000 env
      requireOptionalUnsignedEnv "PERPS_INDEXER_POLL_SECONDS" 1 3_600 env

requireUnsignedEnv
  :: String -> Integer -> Integer -> [(String, String)] -> Either Text ()
requireUnsignedEnv name lower upper env =
  case lookup name env of
    Just raw
      | not (null raw)
          && all isAsciiDigit raw
          && maybe
            False
            (\value -> value >= lower && value <= upper)
            (readMaybe raw) ->
          Right ()
    _ ->
      Left $
        T.pack name
          <> " must be present as an unsigned decimal integer in the supported range"

requireOptionalUnsignedEnv
  :: String -> Integer -> Integer -> [(String, String)] -> Either Text ()
requireOptionalUnsignedEnv name lower upper env =
  case lookup name env of
    Nothing -> Right ()
    Just _ -> requireUnsignedEnv name lower upper env

isAsciiDigit :: Char -> Bool
isAsciiDigit character = character >= '0' && character <= '9'

loadEnvArgs :: IO [(String, String)]
loadEnvArgs = do
  pairs <-
    traverse
      readEnv
      [ "PERPS_INDEXER_RPC_URLS"
      , "PERPS_INDEXER_TRACE_API_URL"
      , "PERPS_CHAIN_ID"
      , "PERPS_CANDLE_WRITE_MODE"
      , "PERPS_INDEXER_START_BLOCK"
      , "PERPS_INDEXER_CONFIRMATIONS"
      , "PERPS_INDEXER_BATCH_SIZE"
      , "PERPS_INDEXER_POLL_SECONDS"
      , "PERPS_USDC"
      , "PERPS_ORDER_ROUTER"
      , "PERPS_ORDER_ROUTER_ADMIN"
      , "PERPS_CFD_ENGINE"
      , "PERPS_CFD_ENGINE_ADMIN"
      , "PERPS_CFD_ENGINE_LENS"
      , "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR"
      , "PERPS_MARGIN_CLEARINGHOUSE"
      , "PERPS_PLETHER_ORACLE"
      , "PERPS_ACCOUNT_LENS"
      , "PERPS_PUBLIC_LENS"
      , "PERPS_HOUSE_POOL"
      , "PERPS_SENIOR_VAULT"
      , "PERPS_JUNIOR_VAULT"
      ]
  pure $ catMaybes pairs
  where
    readEnv name = fmap (\value -> (name, value)) <$> lookupEnv name

parseWorkerArgs
  :: PerpsAddresses
  -> [(String, String)]
  -> [String]
  -> WorkerArgs
parseWorkerArgs addressDefaults env args =
  WorkerArgs
    { waMode = parseMode args
    , waConfirmations =
        readFlag
          "--confirmations"
          (readEnv "PERPS_INDEXER_CONFIRMATIONS" defaultConfirmations)
          args
    , waBatchSize =
        readFlag
          "--batch-size"
          (readEnv "PERPS_INDEXER_BATCH_SIZE" defaultBatchSize)
          args
    , waPollSeconds =
        readFlag
          "--poll-seconds"
          (readEnv "PERPS_INDEXER_POLL_SECONDS" defaultPollSeconds)
          args
    , waStartBlock =
        firstJust
          (lookupFlag "--start-block" args >>= readMaybe)
          (readEnvMaybe "PERPS_INDEXER_START_BLOCK")
    , waRpcUrls =
        case
          firstJust
            (lookupFlag "--rpc-urls" args)
            (lookup "PERPS_INDEXER_RPC_URLS" env)
        of
          Just value -> Just $ splitRpcUrls $ T.pack value
          Nothing -> Nothing
    , waTraceApiUrl =
        T.pack
          <$> firstJust
            (lookupFlag "--trace-api-url" args)
            (lookup "PERPS_INDEXER_TRACE_API_URL" env)
    , waAddresses =
        addressDefaults
          { paUsdc = addressEnv "PERPS_USDC" paUsdc
          , paOrderRouter = addressEnv "PERPS_ORDER_ROUTER" paOrderRouter
          , paOrderRouterAdmin =
              addressEnv "PERPS_ORDER_ROUTER_ADMIN" paOrderRouterAdmin
          , paCfdEngine = addressEnv "PERPS_CFD_ENGINE" paCfdEngine
          , paCfdEngineAdmin =
              addressEnv "PERPS_CFD_ENGINE_ADMIN" paCfdEngineAdmin
          , paCfdEngineLens =
              addressEnv "PERPS_CFD_ENGINE_LENS" paCfdEngineLens
          , paCfdEngineSettlementSidecar =
              addressEnv
                "PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR"
                paCfdEngineSettlementSidecar
          , paMarginClearinghouse =
              addressEnv
                "PERPS_MARGIN_CLEARINGHOUSE"
                paMarginClearinghouse
          , paPletherOracle =
              addressEnv "PERPS_PLETHER_ORACLE" paPletherOracle
          , paAccountLens = addressEnv "PERPS_ACCOUNT_LENS" paAccountLens
          , paPublicLens = addressEnv "PERPS_PUBLIC_LENS" paPublicLens
          , paHousePool = addressEnv "PERPS_HOUSE_POOL" paHousePool
          , paSeniorVault = addressEnv "PERPS_SENIOR_VAULT" paSeniorVault
          , paJuniorVault = addressEnv "PERPS_JUNIOR_VAULT" paJuniorVault
          }
    }
  where
    readEnv name fallback = fromMaybe fallback (lookup name env >>= readMaybe)
    readEnvMaybe name = lookup name env >>= readMaybe
    addressEnv name fieldAccessor =
      T.pack $
        fromMaybe
          (T.unpack $ fieldAccessor addressDefaults)
          (lookup name env)

parseMode :: [String] -> PerpsIndexerMode
parseMode args =
  if "--once" `elem` args
    then PerpsIndexerOnce
    else PerpsIndexerLoop

readFlag :: (Read a) => String -> a -> [String] -> a
readFlag name fallback args =
  case lookupFlag name args >>= readMaybe of
    Just value -> value
    Nothing -> fallback

lookupFlag :: String -> [String] -> Maybe String
lookupFlag _ [] = Nothing
lookupFlag name (flag : value : rest)
  | flag == name = Just value
  | otherwise = lookupFlag name (value : rest)
lookupFlag _ [_] = Nothing

splitRpcUrls :: Text -> [Text]
splitRpcUrls =
  filter (not . T.null)
    . map T.strip
    . T.split (\character ->
        character == ','
          || character == ' '
          || character == '\n'
          || character == '\t'
      )

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just value : rest) = value : catMaybes rest
catMaybes (Nothing : rest) = catMaybes rest

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just value) _ = Just value
firstJust Nothing fallback = fallback
