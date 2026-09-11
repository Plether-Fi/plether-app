-- Read-only, advisory reserve evidence. A conservative reserve is not an
-- estimate for a particular order and cannot prove that order unaffordable.
module Plether.Keeper.Funding
  ( FundingEvidence(..)
  , readFundingEvidence
  , keeperFeeCaps
  , keeperReserveCost
  , classifyKeeperReserve
  ) where

import Control.Concurrent.Async (Concurrently(..), runConcurrently)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256)
import Plether.Ethereum.Client (EthClient, RpcError(..), CallParams(..), ethCallAtBlock)
import Plether.Ethereum.Contracts.Perps (getUpdateFeeCall)
import Plether.Ethereum.Rpc
import Plether.Ethereum.Transaction (applyBpsBuffer)

data FundingEvidence = FundingEvidence
  { fundingBlockTime :: Integer
  , fundingBalance :: Integer
  , fundingConfirmedNonce :: Integer
  , fundingPendingNonce :: Integer
  , fundingGasPrice :: Integer
  , fundingPriorityFee :: Integer
  , fundingUpdateFee :: Integer
  } deriving stock (Eq, Show)

-- These are the same fee caps used by the keeper's transaction builder.
keeperFeeCaps :: Integer -> Integer -> Integer -> (Integer, Integer)
keeperFeeCaps buffer gasPrice priority =
  (applyBpsBuffer (max gasPrice priority) buffer, applyBpsBuffer priority buffer)

keeperReserveCost :: Integer -> Int -> Integer -> FundingEvidence -> Maybe Integer
keeperReserveCost gasCap batchSize feeBuffer FundingEvidence{..}
  | gasCap <= 0 || batchSize <= 0 || feeBuffer < 0
      || fundingGasPrice <= 0 || fundingPriorityFee < 0 || fundingUpdateFee < 0 = Nothing
  | otherwise = Just $ gasCap * maxFee + fromIntegral batchSize * fundingUpdateFee
 where
  (maxFee, _) = keeperFeeCaps feeBuffer fundingGasPrice fundingPriorityFee

-- Unknown pending liabilities never become zero liabilities. Below a generic
-- upper bound means uncertain affordability, not a confirmed execution blocker.
classifyKeeperReserve :: Integer -> FundingEvidence -> Maybe Integer -> (Text, Text)
classifyKeeperReserve now FundingEvidence{..} required
  | fundingBlockTime > now + 2 || now - fundingBlockTime > 15
      || fundingBalance < 0 || fundingConfirmedNonce < 0
      || fundingPendingNonce /= fundingConfirmedNonce = unknown
  | fundingBalance == 0 = ("blocked", "KEEPER_INSUFFICIENT_FUNDS")
  | Just cost <- required, cost > 0 =
      if fundingBalance < cost then ("unknown", "FUNDING_LOW")
      else if fundingBalance < cost * 10 then ("ready", "FUNDING_LOW")
      else ("ready", "READY")
  | otherwise = unknown
 where
  unknown = ("unknown", "FUNDING_UNVERIFIED")

readFundingEvidence :: EthClient -> Text -> Text -> [ByteString] -> IO (Either RpcError FundingEvidence)
readFundingEvidence client signer oracle payload
  | null payload || any BS.null payload = pure $ Left $ RpcJsonError "Funding payload unavailable"
  | otherwise = do
      header <- ethLatestBlock client
      case header of
        Left err -> pure $ Left err
        Right block -> do
          let height = rpcBlockNumber block
          -- Fixed-block account reads; fee and pending-nonce reads must be fresh.
          (balance, confirmed, pending, gasPrice, priority, updateFee) <- runConcurrently $
            (,,,,,) <$> Concurrently (ethGetBalanceAtBlock client signer height)
              <*> Concurrently (ethGetTransactionCountAtBlock client signer height)
              <*> Concurrently (ethGetTransactionCount client signer)
              <*> Concurrently (ethGasPrice client)
              <*> Concurrently (ethMaxPriorityFeePerGas client)
              <*> Concurrently (ethCallAtBlock client (CallParams oracle $ getUpdateFeeCall payload) height)
          canonical <- ethGetBlockByNumber client height
          pure $ do
            verified <- canonical
            if verified /= block then Left $ RpcJsonError "Funding block changed" else Right ()
            feeBytes <- updateFee
            if BS.length feeBytes /= 32 then Left $ RpcJsonError "Funding update fee malformed" else Right ()
            gas <- gasPrice
            -- Match the transaction builder's supported priority-fee fallback.
            let tip = either (const gas) id priority
            FundingEvidence (rpcBlockTimestamp block) <$> balance <*> confirmed <*> pending
              <*> pure gas <*> pure tip <*> pure (decodeUint256 feeBytes)
