module Main (main) where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
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
import Plether.Logging (field, logError, logInfo)
import Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , PerpsIndexerConfig (..)
  , PerpsIndexerMode (..)
  , defaultPerpsAddresses
  , runPerpsIndexer
  )
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
  envArgs <- loadEnvArgs
  eConfig <- loadConfig
  case eConfig of
    Left err ->
      logError
        "perps_indexer_configuration_invalid"
        "Perps indexer configuration is invalid"
        [field "error" err]
    Right cfg ->
      let release = currentProtocolRelease cfg
          configuredAddresses =
            defaultPerpsAddresses
              { paOrderRouter = prOrderRouter release
              , paOrderRouterAdmin = prOrderRouterAdmin release
              , paCfdEngine = prCfdEngine release
              , paCfdEngineAdmin = prCfdEngineAdmin release
              , paMarginClearinghouse = prMarginClearinghouse release
              , paPletherOracle = prPletherOracle release
              , paAccountLens = prAccountLens release
              , paPublicLens = prPublicLens release
              , paHousePool = prHousePool release
              , paSeniorVault = prSeniorVault release
              , paJuniorVault = prJuniorVault release
              }
          args = parseWorkerArgs configuredAddresses envArgs cliArgs
       in case cfgDatabaseUrl cfg of
        Nothing ->
          logError
            "perps_indexer_database_missing"
            "Perps indexer requires a database"
            []
        Just dbUrl -> do
          manager <- newManager tlsManagerSettings
          let rpcUrls = fromMaybe [cfgPerpsRpcUrl cfg] (waRpcUrls args)
          probeResults <- forM rpcUrls $ \rpcUrl -> do
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
                [ field "configured_provider_count" $ length rpcUrls
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
                args
                matchingRpcUrls

startValidatedIndexer
  :: Manager
  -> Text
  -> Config
  -> ProtocolRelease
  -> WorkerArgs
  -> [Text]
  -> IO ()
startValidatedIndexer manager dbUrl cfg release args rpcUrls = do
  pool <- newDbPool dbUrl
  withDb pool ensurePerpsHistorySchema
  withDb pool $ \conn -> ensureProtocolSchema conn release
  let -- A release cursor certifies completeness only when its first
      -- projection starts at the manifest deployment block. Bounded
      -- operator backfills use PerpsIndexerBackfill's explicit range;
      -- they do not move this release floor.
      startBlock = prDeploymentBlock release
      requestedStartBlock =
        fromMaybe (cfgPerpsIndexerStartBlock cfg) (waStartBlock args)
      indexerCfg =
        PerpsIndexerConfig
          { picRpcUrls = rpcUrls
          , picChainId = prChainId release
          , picReleaseId = prId release
          , picCalculationVersion = prCalculationVersion release
          , picAddresses = waAddresses args
          , picStartBlock = startBlock
          , picConfirmations = waConfirmations args
          , picBatchSize = waBatchSize args
          , picPollIntervalMicros = max 1 (waPollSeconds args) * 1_000_000
          , picIndexerName = "perps-history"
          , picMode = waMode args
          }
  logInfo
    "perps_indexer_started"
    "Perps history indexer started with release-bound RPC providers"
    [ field "mode" $ show $ waMode args
    , field "start_block" startBlock
    , field "requested_start_block" requestedStartBlock
    , field "confirmations" $ waConfirmations args
    , field "batch_size" $ waBatchSize args
    , field "poll_seconds" $ waPollSeconds args
    , field "rpc_provider_count" $ length rpcUrls
    ]
  runPerpsIndexer manager pool indexerCfg

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

loadEnvArgs :: IO [(String, String)]
loadEnvArgs = do
  pairs <- traverse readEnv
    [ "PERPS_INDEXER_RPC_URLS"
    , "PERPS_INDEXER_START_BLOCK"
    , "PERPS_INDEXER_CONFIRMATIONS"
    , "PERPS_INDEXER_BATCH_SIZE"
    , "PERPS_INDEXER_POLL_SECONDS"
    ]
  pure $ catMaybes pairs
  where
    readEnv name = fmap (\value -> (name, value)) <$> lookupEnv name

parseWorkerArgs :: PerpsAddresses -> [(String, String)] -> [String] -> WorkerArgs
parseWorkerArgs addressDefaults env args =
  WorkerArgs
    { waMode = parseMode args
    , waConfirmations = readFlag "--confirmations" (readEnv "PERPS_INDEXER_CONFIRMATIONS" defaultConfirmations) args
    , waBatchSize = readFlag "--batch-size" (readEnv "PERPS_INDEXER_BATCH_SIZE" defaultBatchSize) args
    , waPollSeconds = readFlag "--poll-seconds" (readEnv "PERPS_INDEXER_POLL_SECONDS" defaultPollSeconds) args
    , waStartBlock =
        firstJust
          (lookupFlag "--start-block" args >>= readMaybe)
          (readEnvMaybe "PERPS_INDEXER_START_BLOCK")
    , waRpcUrls =
        case firstJust (lookupFlag "--rpc-urls" args) (lookup "PERPS_INDEXER_RPC_URLS" env) of
          Just value -> Just $ splitRpcUrls $ T.pack value
          Nothing -> Nothing
    , waAddresses = addressDefaults
    }
  where
    readEnv name fallback = fromMaybe fallback (lookup name env >>= readMaybe)
    readEnvMaybe name = lookup name env >>= readMaybe

parseMode :: [String] -> PerpsIndexerMode
parseMode args =
  case (lookupFlag "--from" args >>= readMaybe, lookupFlag "--to" args >>= readMaybe) of
    (Just fromBlock, Just toBlock) | "--backfill" `elem` args -> PerpsIndexerBackfill fromBlock toBlock
    _ | "--once" `elem` args -> PerpsIndexerOnce
    _ -> PerpsIndexerLoop

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
    . T.split (\c -> c == ',' || c == ' ' || c == '\n' || c == '\t')

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just value : rest) = value : catMaybes rest
catMaybes (Nothing : rest) = catMaybes rest

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just value) _ = Just value
firstJust Nothing fallback = fallback
