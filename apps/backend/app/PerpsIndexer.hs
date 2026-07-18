module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (newDbPool, withDb)
import Plether.Database.Schema (ensurePerpsHistorySchema)
import Plether.Logging (field, logError, logInfo)
import Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , PerpsIndexerConfig (..)
  , PerpsIndexerMode (..)
  , defaultPerpsAddresses
  , runPerpsIndexer
  )
import System.Environment (getArgs, lookupEnv)
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
defaultConfirmations = 120

defaultBatchSize :: Integer
defaultBatchSize = 5000

defaultPollSeconds :: Int
defaultPollSeconds = 12

main :: IO ()
main = do
  cliArgs <- getArgs
  envArgs <- loadEnvArgs
  let args = parseWorkerArgs envArgs cliArgs
  eConfig <- loadConfig
  case eConfig of
    Left err ->
      logError
        "perps_indexer_configuration_invalid"
        "Perps indexer configuration is invalid"
        [field "error" err]
    Right cfg ->
      case cfgDatabaseUrl cfg of
        Nothing ->
          logError
            "perps_indexer_database_missing"
            "Perps indexer requires a database"
            []
        Just dbUrl -> do
          manager <- newManager tlsManagerSettings
          pool <- newDbPool dbUrl
          withDb pool ensurePerpsHistorySchema
          let rpcUrls = fromMaybe [cfgRpcUrl cfg] (waRpcUrls args)
              startBlock = fromMaybe (cfgPerpsIndexerStartBlock cfg) (waStartBlock args)
              indexerCfg =
                PerpsIndexerConfig
                  { picRpcUrls = rpcUrls
                  , picChainId = cfgChainId cfg
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
            "Perps history indexer started"
            [ field "mode" $ show $ waMode args
            , field "start_block" startBlock
            , field "confirmations" $ waConfirmations args
            , field "batch_size" $ waBatchSize args
            , field "poll_seconds" $ waPollSeconds args
            , field "rpc_provider_count" $ maybe 1 length $ waRpcUrls args
            ]
          runPerpsIndexer manager pool indexerCfg

loadEnvArgs :: IO [(String, String)]
loadEnvArgs = do
  pairs <- traverse readEnv
    [ "PERPS_INDEXER_RPC_URLS"
    , "PERPS_INDEXER_START_BLOCK"
    , "PERPS_INDEXER_CONFIRMATIONS"
    , "PERPS_INDEXER_BATCH_SIZE"
    , "PERPS_INDEXER_POLL_SECONDS"
    , "PERPS_ORDER_ROUTER"
    , "PERPS_CFD_ENGINE"
    , "PERPS_MARGIN_CLEARINGHOUSE"
    ]
  pure $ catMaybes pairs
  where
    readEnv name = fmap (\value -> (name, value)) <$> lookupEnv name

parseWorkerArgs :: [(String, String)] -> [String] -> WorkerArgs
parseWorkerArgs env args =
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
    , waAddresses =
        defaultPerpsAddresses
          { paOrderRouter = T.pack $ fromMaybe (T.unpack $ paOrderRouter defaultPerpsAddresses) (lookup "PERPS_ORDER_ROUTER" env)
          , paCfdEngine = T.pack $ fromMaybe (T.unpack $ paCfdEngine defaultPerpsAddresses) (lookup "PERPS_CFD_ENGINE" env)
          , paMarginClearinghouse = T.pack $ fromMaybe (T.unpack $ paMarginClearinghouse defaultPerpsAddresses) (lookup "PERPS_MARGIN_CLEARINGHOUSE" env)
          }
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
