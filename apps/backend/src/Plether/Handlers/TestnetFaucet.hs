module Plether.Handlers.TestnetFaucet
  ( TestnetFaucetResponse (..)
  , claimTestnetFaucet
  , testnetFaucetAmount
  , testnetFaucetEnabled
  , faucetMintCall
  ) where

import Control.Concurrent (threadDelay)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import Plether.Config (Config (..))
import Plether.Database (DbPool, withDb)
import Plether.Database.Schema
  ( TestnetFaucetClaimRow (..)
  , beginTestnetFaucetClaim
  , getTestnetFaucetClaim
  , markTestnetFaucetClaimFailed
  , markTestnetFaucetClaimSuccess
  )
import Plether.Ethereum.Abi (encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (EthClient, RpcError (..), ethBlockNumber)
import Plether.Ethereum.Rpc
  ( TxReceipt (..)
  , ethEstimateGas
  , ethGasPrice
  , ethGetTransactionCount
  , ethGetTransactionReceipt
  , ethMaxPriorityFeePerGas
  , ethSendRawTransaction
  )
import Plether.Ethereum.Transaction
  ( SignedTransaction (..)
  , Tx1559 (..)
  , deriveAddress
  , signTransaction
  )
import Plether.Types (ApiError, ApiResponse, mkResponse)
import qualified Plether.Types.Error as E

testnetFaucetAmount :: Integer
testnetFaucetAmount = 100_000 * 1_000_000

data TestnetFaucetResponse = TestnetFaucetResponse
  { tfrAddress :: Text
  , tfrAmount :: Integer
  , tfrToken :: Text
  , tfrTxHash :: Text
  , tfrStatus :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TestnetFaucetResponse where
  toJSON TestnetFaucetResponse {..} =
    object
      [ "address" .= tfrAddress
      , "amount" .= show tfrAmount
      , "token" .= tfrToken
      , "txHash" .= tfrTxHash
      , "status" .= tfrStatus
      ]

testnetFaucetEnabled :: Config -> Bool
testnetFaucetEnabled cfg = cfgPerpsChainId cfg == 421614

faucetMintCall :: Text -> ByteString
faucetMintCall recipient =
  encodeCall "mint(address,uint256)"
    [ encodeAddress recipient
    , encodeUint256 testnetFaucetAmount
    ]

claimTestnetFaucet
  :: DbPool
  -> EthClient
  -> Config
  -> Text
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
claimTestnetFaucet pool client cfg rawAddress
  | not (testnetFaucetEnabled cfg) =
      pure $ Left $ E.internalError "Testnet faucet is only available for Arbitrum Sepolia perps"
  | otherwise =
      case cfgFaucetPrivateKey cfg of
        Nothing ->
          pure $ Left $ E.internalError "FAUCET_PRIVATE_KEY is not configured"
        Just privateKey -> do
          let address = T.toLower rawAddress
              token = T.toLower $ cfgPerpsUsdc cfg
          existing <- withDb pool $ \conn -> getTestnetFaucetClaim conn address token
          case existing of
            Just claim | tfcStatus claim == "success" ->
              alreadyClaimedResponse client (cfgPerpsChainId cfg) address token claim
            Just claim | tfcStatus claim == "pending" ->
              pure $ Left $ E.mkError E.RateLimited "Faucet claim is already in progress for this address"
            _ -> do
              started <- withDb pool $ \conn -> beginTestnetFaucetClaim conn address testnetFaucetAmount token
              if not started
                then do
                  claim <- withDb pool $ \conn -> getTestnetFaucetClaim conn address token
                  case claim of
                    Just row | tfcStatus row == "success" ->
                      alreadyClaimedResponse client (cfgPerpsChainId cfg) address token row
                    _ ->
                      pure $ Left $ E.mkError E.RateLimited "Faucet claim is already in progress for this address"
                else do
                  submitResult <- submitFaucetMint cfg client privateKey token address
                  case submitResult of
                    Left err -> do
                      withDb pool $ \conn -> markTestnetFaucetClaimFailed conn address token err
                      pure $ Left $ E.rpcError err
                    Right receipt -> do
                      if receiptSucceeded receipt
                        then do
                          withDb pool $ \conn -> markTestnetFaucetClaimSuccess conn address token (receiptTxHash receipt)
                          pure $
                            Right $
                              mkResponse
                                (receiptBlockNumber receipt)
                                (cfgPerpsChainId cfg)
                                (faucetResponse address token (receiptTxHash receipt) "minted")
                        else do
                          let err = "faucet mint transaction reverted: " <> receiptTxHash receipt
                          withDb pool $ \conn -> markTestnetFaucetClaimFailed conn address token err
                          pure $ Left $ E.rpcError err

alreadyClaimedResponse
  :: EthClient
  -> Integer
  -> Text
  -> Text
  -> TestnetFaucetClaimRow
  -> IO (Either ApiError (ApiResponse TestnetFaucetResponse))
alreadyClaimedResponse client chainId address token claim =
  case tfcTxHash claim of
    Just txHash -> do
      blockResult <- ethBlockNumber client
      let blockNum = either (const 0) id blockResult
      pure $
        Right $
          mkResponse blockNum chainId (faucetResponse address token txHash "already_claimed")
    Nothing ->
      pure $ Left $ E.internalError "Faucet claim is marked successful without a transaction hash"

faucetResponse :: Text -> Text -> Text -> Text -> TestnetFaucetResponse
faucetResponse address token txHash status =
  TestnetFaucetResponse
    { tfrAddress = address
    , tfrAmount = testnetFaucetAmount
    , tfrToken = token
    , tfrTxHash = txHash
    , tfrStatus = status
    }

submitFaucetMint :: Config -> EthClient -> Text -> Text -> Text -> IO (Either Text TxReceipt)
submitFaucetMint cfg client privateKey token recipient =
  deriveAddress privateKey >>= \case
    Left err -> pure $ Left err
    Right fromAddr -> do
      let callData = faucetMintCall recipient
      nonceResult <- ethGetTransactionCount client fromAddr
      gasResult <- ethEstimateGas client fromAddr token 0 callData
      gasPriceResult <- ethGasPrice client
      priorityResult <- ethMaxPriorityFeePerGas client
      case (nonceResult, gasResult, gasPriceResult) of
        (Right nonce, Right estimatedGas, Right gasPrice) -> do
          let priorityBase = either (const gasPrice) id priorityResult
              maxFeeBase = max gasPrice priorityBase
              gasLimit = max 21_000 $ applyBuffer estimatedGas (cfgKeeperGasBufferBps cfg)
              maxPriorityFee = applyBuffer priorityBase (cfgKeeperFeeBufferBps cfg)
              maxFee = max maxPriorityFee $ applyBuffer maxFeeBase (cfgKeeperFeeBufferBps cfg)
              tx =
                Tx1559
                  { txChainId = cfgPerpsChainId cfg
                  , txNonce = nonce
                  , txMaxPriorityFeePerGas = maxPriorityFee
                  , txMaxFeePerGas = maxFee
                  , txGasLimit = gasLimit
                  , txTo = token
                  , txValue = 0
                  , txData = callData
                  }
          signResult <- signTransaction privateKey tx
          case signResult of
            Left err -> pure $ Left err
            Right signed -> do
              sendResult <- ethSendRawTransaction client (signedRawTransaction signed)
              case sendResult of
                Left err -> pure $ Left $ rpcErrorText err
                Right txHash -> waitForReceipt client txHash 60
        _ ->
          pure $
            Left $
              T.intercalate
                "; "
                $ concat
                  [ either ((: []) . rpcErrorText) (const []) nonceResult
                  , either ((: []) . rpcErrorText) (const []) gasResult
                  , either ((: []) . rpcErrorText) (const []) gasPriceResult
                  ]

waitForReceipt :: EthClient -> Text -> Int -> IO (Either Text TxReceipt)
waitForReceipt _ txHash 0 = pure $ Left $ "timed out waiting for receipt " <> txHash
waitForReceipt client txHash attempts = do
  receiptResult <- ethGetTransactionReceipt client txHash
  case receiptResult of
    Left err -> pure $ Left $ rpcErrorText err
    Right (Just receipt) -> pure $ Right receipt
    Right Nothing -> do
      threadDelay 2_000_000
      waitForReceipt client txHash (attempts - 1)

applyBuffer :: Integer -> Integer -> Integer
applyBuffer value bufferBps = (value * (10_000 + bufferBps)) `div` 10_000

rpcErrorText :: RpcError -> Text
rpcErrorText = \case
  RpcHttpError msg -> msg
  RpcJsonError msg -> msg
  RpcNodeError _ msg _ -> msg
