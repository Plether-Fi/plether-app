module Main (main) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Plether.Config (Config (..), loadConfig)
import Plether.Ethereum.Abi (encodeCall)
import Plether.Ethereum.Client (CallParams (..), EthClient, ethCallAtBlock, newClient, rpcCall)
import Plether.Ethereum.Rpc (RpcLog (..), ethChainId, ethGetLogs)
import Plether.Logging (field, logError, logInfo)
import Plether.Vaults.ActivityIndexer
  ( depositRequestTopic
  , legacyDepositRequestedTopic
  , redeemRequestTopic
  , transferTopic
  )
import System.Exit (exitFailure)

main :: IO ()
main = do
  loaded <- loadConfig
  case loaded of
    Left err -> failPreflight err
    Right cfg -> do
      client <- newClient $ cfgPerpsRpcUrl cfg
      result <- runProviderPreflight client cfg
      case result of
        Left err -> failPreflight $ T.unpack err
        Right tracedTx ->
          logInfo
            "alchemy_provider_preflight_succeeded"
            "Alchemy passed the required vault, Bundler, archive, and Debug probes"
            [ field "chain_id" $ cfgPerpsChainId cfg
            , field "deployment_block" $ cfgVaultHistoryDeploymentBlock cfg
            , field "trace_transaction_hash" tracedTx
            ]

failPreflight :: String -> IO a
failPreflight err = do
  logError
    "alchemy_provider_preflight_failed"
    "Alchemy provider preflight failed"
    [field "error" err]
  exitFailure

runProviderPreflight :: EthClient -> Config -> IO (Either T.Text T.Text)
runProviderPreflight client cfg = do
  chain <- ethChainId client
  case chain of
    Left err -> pure $ Left $ "eth_chainId failed: " <> T.pack (show err)
    Right chainId
      | chainId /= 421614 -> pure $ Left "PERPS_RPC_URL must report chain ID 421614"
      | chainId /= cfgPerpsChainId cfg -> pure $ Left "PERPS_RPC_URL does not match PERPS_CHAIN_ID"
      | otherwise -> do
          historical <- traverse (verifyHistoricalVaultCall client cfg) configuredVaults
          case firstLeft historical of
            Just err -> pure $ Left err
            Nothing -> do
              logs <- traverse (readVaultLogs client cfg) configuredVaults
              case firstLeft logs of
                Just err -> pure $ Left err
                Nothing -> do
                  receipt <-
                    rpcCall
                      client
                      "eth_getUserOperationReceipt"
                      (toJSON [String $ "0x" <> T.replicate 64 "0"])
                  case receipt of
                    Left err -> pure $ Left $ "eth_getUserOperationReceipt failed: " <> T.pack (show err)
                    Right value | value /= Null ->
                      pure $ Left "Unknown UserOperation receipt probe returned unexpected evidence"
                    Right _ ->
                      case concat [entries | Right entries <- logs] of
                        [] -> pure $ Left "Vault log probe returned no known confirmed transaction to trace"
                        entry : _ -> do
                          trace <-
                            rpcCall
                              client
                              "debug_traceTransaction"
                              (toJSON
                                [ String $ rpcLogTxHash entry
                                , object
                                    [ "tracer" .= ("callTracer" :: T.Text)
                                    , "timeout" .= ("20s" :: T.Text)
                                    ]
                                ])
                          pure $ case trace of
                            Left err -> Left $ "debug_traceTransaction failed: " <> T.pack (show err)
                            Right (Object _) -> Right $ rpcLogTxHash entry
                            Right _ -> Left "debug_traceTransaction returned a non-object call trace"
 where
  configuredVaults =
    [ cfgVaultHistorySeniorVaultAddress cfg
    , cfgVaultHistoryJuniorVaultAddress cfg
    ]

verifyHistoricalVaultCall :: EthClient -> Config -> T.Text -> IO (Either T.Text ())
verifyHistoricalVaultCall client cfg vault = do
  results <-
    traverse
      (\signature ->
        ethCallAtBlock
          client
          CallParams
            { callTo = vault
            , callData = encodeCall signature []
            }
          (cfgVaultHistoryDeploymentBlock cfg))
      ["totalAssets()", "totalSupply()"]
  pure $ case firstLeft results of
    Just err -> Left $ "Historical vault eth_call failed: " <> T.pack (show err)
    Nothing ->
      let payloads = [payload | Right payload <- results]
       in if all ((== 32) . BS.length) payloads
            then Right ()
            else Left "Historical vault eth_call returned malformed uint256 data"

readVaultLogs :: EthClient -> Config -> T.Text -> IO (Either T.Text [RpcLog])
readVaultLogs client cfg vault = do
  let fromBlock = cfgVaultHistoryDeploymentBlock cfg
      toBlock = fromBlock + 4_999
      topics = [transferTopic, depositRequestTopic, redeemRequestTopic, legacyDepositRequestedTopic]
  result <- ethGetLogs client vault topics fromBlock toBlock
  pure $ case result of
    Left err -> Left $ "Vault eth_getLogs probe failed: " <> T.pack (show err)
    Right entries -> Right entries

firstLeft :: [Either a b] -> Maybe a
firstLeft [] = Nothing
firstLeft (Left err : _) = Just err
firstLeft (Right _ : rest) = firstLeft rest
