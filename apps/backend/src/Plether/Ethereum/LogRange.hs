{-# LANGUAGE TemplateHaskell #-}

-- | Bounded, read-only log-range recovery. Only recognized size/range failures
-- may subdivide a range; an unavailable provider must not trigger a request tree.
module Plether.Ethereum.LogRange
  ( RpcFailureCategory (..)
  , maxLogRangeBlocks
  , defaultReconcilerBatchBlocks
  , rpcFailureCategory
  , rpcFailureCode
  , scanLogRange
  , reconcilerRetryDelayMicros
  ) where

import Control.Monad (unless)
import Data.Aeson (Result (..), Value (..), eitherDecodeFileStrict', fromJSON)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)
import Plether.Ethereum.Client (RpcError (..))
import System.Directory (doesFileExist, makeAbsolute)

-- Compile the same policy used by the gateway and Terraform into the binary.
-- There is no runtime fallback to a stale or absent policy file.
logRangePolicy :: (Integer, Integer)
logRangePolicy = $(do
  let repositoryPath = "../../config/rpc-log-range-policy.json"
      dockerPath = "config/rpc-log-range-policy.json"
  inRepository <- runIO $ doesFileExist repositoryPath
  path <- runIO $ makeAbsolute $ if inRepository then repositoryPath else dockerPath
  addDependentFile path
  decoded <- runIO $ eitherDecodeFileStrict' path
  value <- either fail pure decoded
  case value of
    Object fields -> do
      let integer key = case KM.lookup key fields of
            Just raw -> case fromJSON raw :: Result Integer of
              Success n -> pure n
              Error err -> fail err
            Nothing -> fail "Missing RPC log-range policy field"
      version <- integer "version"
      maximumBlocks <- integer "maxLogRangeBlocks"
      defaultBlocks <- integer "reconcilerDefaultBatchBlocks"
      unless (version == 1 && defaultBlocks > 0 && defaultBlocks <= maximumBlocks) $
        fail "Unsupported RPC log-range policy"
      lift (maximumBlocks, defaultBlocks)
    _ -> fail "Invalid RPC log-range policy")

maxLogRangeBlocks :: Integer
maxLogRangeBlocks = fst logRangePolicy

defaultReconcilerBatchBlocks :: Integer
defaultReconcilerBatchBlocks = snd logRangePolicy

data RpcFailureCategory
  = RpcRangeLimit
  | RpcRateLimit
  | RpcTimeout
  | RpcUnavailable
  | RpcInvalidResponse
  deriving stock (Eq, Show)

-- Provider text is inspected only for classification and is never returned or
-- logged. Ambiguous error codes (including -32005) alone do not authorize splits.
rpcFailureCategory :: RpcError -> RpcFailureCategory
rpcFailureCategory = \case
  RpcHttpError message
    | message == "RPC_RANGE_LIMIT"
        || message == "Ethereum RPC response exceeded the configured size limit"
        || "statuscode = 413" `T.isInfixOf` T.toLower message -> RpcRangeLimit
    | "statuscode = 429" `T.isInfixOf` T.toLower message -> RpcRateLimit
    | "timeout" `T.isInfixOf` T.toLower message -> RpcTimeout
    | otherwise -> RpcUnavailable
  RpcNodeError code message _
    | code == 429 || contains ["rate limit", "too many requests", "throughput"] message -> RpcRateLimit
    | code `elem` [-32602, -32000, -32005, -32600]
        && contains ["block range", "response size exceeded", "log response size", "too many results", "query returned more than"] message -> RpcRangeLimit
    | contains ["timeout", "timed out"] message -> RpcTimeout
    | otherwise -> RpcUnavailable
  RpcJsonError _ -> RpcInvalidResponse
 where
  contains needles raw = any (`T.isInfixOf` T.toLower raw) needles

rpcFailureCode :: RpcFailureCategory -> Text
rpcFailureCode = \case
  RpcRangeLimit -> "RPC_RANGE_LIMIT"
  RpcRateLimit -> "RPC_RATE_LIMIT"
  RpcTimeout -> "RPC_TIMEOUT"
  RpcUnavailable -> "RPC_UNAVAILABLE"
  RpcInvalidResponse -> "RPC_INVALID_RESPONSE"

-- | Preserve the complete inclusive interval or return failure. The budget of
-- 63 requests permits 32 leaves, while bounding hostile or tiny provider limits.
-- A failed child discards all partial results; the caller cannot advance its
-- cursor until the entire range has been agreed and canonically revalidated.
scanLogRange
  :: Monad m
  => (Integer -> Integer -> m (Either RpcFailureCategory [a]))
  -> Integer
  -> Integer
  -> m (Either RpcFailureCategory [a])
scanLogRange fetch first lastBlock
  | first < 0 || lastBlock < first || lastBlock - first + 1 > maxLogRangeBlocks =
      pure $ Left RpcRangeLimit
  | otherwise = fst <$> go 63 first lastBlock
 where
  go remaining fromBlock toBlock
    | remaining <= (0 :: Int) = pure (Left RpcRangeLimit, 0)
    | otherwise = do
        result <- fetch fromBlock toBlock
        let budget = remaining - 1
        case result of
          Left RpcRangeLimit | fromBlock < toBlock -> do
            let middle = fromBlock + (toBlock - fromBlock) `div` 2
            (left, afterLeft) <- go budget fromBlock middle
            case left of
              Left err -> pure (Left err, afterLeft)
              Right leftEvents -> do
                (right, afterRight) <- go afterLeft (middle + 1) toBlock
                pure ((leftEvents <>) <$> right, afterRight)
          _ -> pure (result, budget)

-- The circuit breaker still observes every failed cycle. Backoff only changes
-- the next read's cadence; it never changes freshness or auto-clears a pause.
reconcilerRetryDelayMicros :: Int -> Int -> Int -> Text -> Int
reconcilerRetryDelayMicros pollSeconds failures jitterMicros reason
  | any (`T.isInfixOf` reason) ["RPC_RATE_LIMIT", "RPC_TIMEOUT", "RPC_UNAVAILABLE"] =
      min 60_000_000 $ base * 2 ^ min 5 (max 0 (failures - 1)) + max 0 (min 250_000 jitterMicros)
  | otherwise = base
 where
  base = max 1 (min 60 pollSeconds) * 1_000_000
