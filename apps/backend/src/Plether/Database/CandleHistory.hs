module Plether.Database.CandleHistory
  ( CandleHistorySelection (..)
  , MarketReleaseEpoch (..)
  , candleHistorySchemaStatements
  , effectiveHistoryStart
  , ensureCandleHistorySchema
  , releaseEpochAtBlock
  , validateCandleHistorySelection
  , validateMarketReleaseEpoch
  , validateMarketReleaseEpochSequence
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isAsciiLower)
import Data.List (find, nub)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Query, execute_)

-- | An immutable operator request. The selected timestamp is intentionally not
-- bucket-aligned: every interval begins at its first complete canonical bucket.
data CandleHistorySelection = CandleHistorySelection
  { chsMarketId :: Text
  , chsRevision :: Integer
  , chsRequestedStartTimestamp :: Integer
  , chsRequestedBy :: Text
  , chsRequestReference :: Text
  }
  deriving stock (Eq, Show)

-- | Immutable contract provenance for one release of a logical market. The
-- release owns blocks from its activation block through the block immediately
-- before the next release activation. The exclusive end is derived, not stored.
data MarketReleaseEpoch = MarketReleaseEpoch
  { mreMarketId :: Text
  , mreRevision :: Integer
  , mreChainId :: Integer
  , mreReleaseRouter :: Text
  , mreCfdEngine :: Text
  , mreMarginClearinghouse :: Text
  , mreDeploymentBlock :: Integer
  , mreDeploymentBlockHash :: Text
  , mreDeploymentTransactionHash :: Text
  , mreActivationBlock :: Integer
  , mreActivationTimestamp :: Integer
  , mreActivationBlockHash :: Text
  , mreApprovalReference :: Text
  , mreIsMarketGenesis :: Bool
  }
  deriving stock (Eq, Show)

ensureCandleHistorySchema :: Connection -> IO ()
ensureCandleHistorySchema conn =
  forM_ candleHistorySchemaStatements $ \statement -> do
    _ <- execute_ conn statement
    pure ()

-- | Align an arbitrary selected second to the first complete bucket which does
-- not begin before it. Source availability is deliberately not inferred here.
effectiveHistoryStart :: Integer -> Integer -> Either Text Integer
effectiveHistoryStart interval requestedStart
  | interval <= 0 = Left "Candle interval must be positive"
  | requestedStart < 0 = Left "Candle history start cannot precede the Unix epoch"
  | otherwise =
      Right $ ((requestedStart + interval - 1) `div` interval) * interval

validateCandleHistorySelection :: CandleHistorySelection -> Either Text ()
validateCandleHistorySelection CandleHistorySelection {..} = do
  validateMarketId chsMarketId
  when (chsRevision <= 0) $
    Left "Candle history request revision must be positive"
  when (chsRequestedStartTimestamp < 0) $
    Left "Candle history start cannot precede the Unix epoch"
  requireNonBlank "Candle history requester" chsRequestedBy
  requireNonBlank "Candle history request reference" chsRequestReference

validateMarketReleaseEpoch :: MarketReleaseEpoch -> Either Text ()
validateMarketReleaseEpoch MarketReleaseEpoch {..} = do
  validateMarketId mreMarketId
  when (mreRevision <= 0) $ Left "Market release revision must be positive"
  when (mreIsMarketGenesis /= (mreRevision == 1)) $
    Left "Only market release revision one may be the market genesis"
  when (mreChainId <= 0) $ Left "Market release chain id must be positive"
  validateCanonicalHex "Market release router" 40 mreReleaseRouter
  validateCanonicalHex "Market release CFD engine" 40 mreCfdEngine
  validateCanonicalHex
    "Market release margin clearinghouse"
    40
    mreMarginClearinghouse
  validateCanonicalHex "Deployment block hash" 64 mreDeploymentBlockHash
  validateCanonicalHex
    "Deployment transaction hash"
    64
    mreDeploymentTransactionHash
  validateCanonicalHex "Activation block hash" 64 mreActivationBlockHash
  requireNonBlank "Market release approval reference" mreApprovalReference
  when (mreDeploymentBlock <= 0) $
    Left "Market release deployment block must be positive"
  when (mreActivationBlock < mreDeploymentBlock) $
    Left "Market release activation cannot precede deployment"
  when (mreActivationTimestamp < 0) $
    Left "Market release activation timestamp cannot be negative"

-- | Validate a complete release registry in activation order. New releases are
-- appended in this order, and block ownership is derived from adjacent rows.
validateMarketReleaseEpochSequence :: [MarketReleaseEpoch] -> Either Text ()
validateMarketReleaseEpochSequence [] =
  Left "Market release sequence must contain a genesis epoch"
validateMarketReleaseEpochSequence epochs@(firstEpoch : laterEpochs) = do
  mapM_ validateMarketReleaseEpoch epochs
  unless (mreIsMarketGenesis firstEpoch) $
    Left "The first market release epoch must be the market genesis"
  when (any mreIsMarketGenesis laterEpochs) $
    Left "Only the first market release epoch may be the market genesis"
  unless (all ((== mreMarketId firstEpoch) . mreMarketId) laterEpochs) $
    Left "Market release epochs must belong to one logical market"
  unless (all ((== mreChainId firstEpoch) . mreChainId) laterEpochs) $
    Left "Market release epochs must belong to one chain"
  unless (map mreRevision epochs == [1 .. fromIntegral (length epochs)]) $
    Left "Market release revisions must be contiguous from one"
  unless (strictlyIncreasing $ map mreActivationBlock epochs) $
    Left "Market release activation blocks must be strictly increasing"
  unless (nondecreasing $ map mreActivationTimestamp epochs) $
    Left "Market release activation timestamps must be nondecreasing"
  unless (allUnique $ map mreReleaseRouter epochs) $
    Left "A release router cannot own more than one epoch in a logical market"

-- | Resolve half-open release ownership. At a transition block the successor
-- owns the block, preventing overlap or double counting.
releaseEpochAtBlock
  :: [MarketReleaseEpoch]
  -> Integer
  -> Either Text (Maybe MarketReleaseEpoch)
releaseEpochAtBlock epochs blockNumber = do
  when (blockNumber < 0) $ Left "Market release lookup block cannot be negative"
  validateMarketReleaseEpochSequence epochs
  pure $
    find
      ((<= blockNumber) . mreActivationBlock)
      (reverse epochs)

validateMarketId :: Text -> Either Text ()
validateMarketId marketId
  | T.null marketId || T.length marketId > 63 =
      Left "Market id must contain from 1 through 63 characters"
  | not (isMarketIdHead $ T.head marketId) =
      Left "Market id must begin with a lowercase letter or digit"
  | not (T.all isMarketIdCharacter marketId) =
      Left "Market id may contain only lowercase letters, digits, and hyphens"
  | otherwise = Right ()
 where
  isMarketIdHead char = isAsciiLower char || isAsciiDigit char
  isMarketIdCharacter char = isMarketIdHead char || char == '-'

validateCanonicalHex :: Text -> Int -> Text -> Either Text ()
validateCanonicalHex label digitCount value
  | T.length value /= digitCount + 2
      || T.take 2 value /= "0x"
      || not (T.all isLowerHexDigit $ T.drop 2 value)
      || T.all (== '0') (T.drop 2 value) =
      Left $ label <> " must be canonical lowercase 0x-prefixed hex"
  | otherwise = Right ()
 where
  isLowerHexDigit char = isAsciiDigit char || char >= 'a' && char <= 'f'

isAsciiDigit :: Char -> Bool
isAsciiDigit char = char >= '0' && char <= '9'

requireNonBlank :: Text -> Text -> Either Text ()
requireNonBlank label value =
  when (T.null $ T.strip value) $ Left $ label <> " cannot be blank"

strictlyIncreasing :: Ord value => [value] -> Bool
strictlyIncreasing values = and $ zipWith (<) values $ drop 1 values

nondecreasing :: Ord value => [value] -> Bool
nondecreasing values = and $ zipWith (<=) values $ drop 1 values

allUnique :: Eq value => [value] -> Bool
allUnique values = length values == length (nub values)

candleHistorySchemaStatements :: [Query]
candleHistorySchemaStatements =
  [ "CREATE TABLE IF NOT EXISTS perps_candle_markets (\
    \market_id TEXT PRIMARY KEY, chain_id BIGINT NOT NULL, \
    \price_series_id TEXT NOT NULL REFERENCES perps_basket_definitions(series_id), \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \UNIQUE (market_id, chain_id), CHECK (chain_id > 0), \
    \CHECK (market_id ~ '^[a-z0-9][a-z0-9-]{0,62}$'))"
  , "CREATE OR REPLACE FUNCTION protect_perps_candle_market_identity() \
    \RETURNS TRIGGER LANGUAGE plpgsql AS $candle_market_identity$ BEGIN \
    \IF TG_OP = 'INSERT' THEN NEW.created_at := NOW(); RETURN NEW; END IF; \
    \RAISE EXCEPTION 'candle market identity is immutable' USING ERRCODE = '55000'; \
    \END $candle_market_identity$"
  , "DO $candle_market_triggers$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger \
    \ WHERE tgname = 'perps_candle_market_immutable' \
    \ AND tgrelid = 'perps_candle_markets'::regclass) THEN \
    \ CREATE TRIGGER perps_candle_market_immutable BEFORE INSERT OR UPDATE OR DELETE \
    \ ON perps_candle_markets FOR EACH ROW \
    \ EXECUTE FUNCTION protect_perps_candle_market_identity(); \
    \END IF; END $candle_market_triggers$"
  , "CREATE TABLE IF NOT EXISTS perps_candle_history_targets (\
    \market_id TEXT NOT NULL REFERENCES perps_candle_markets(market_id), \
    \revision BIGINT NOT NULL, requested_start_timestamp BIGINT NOT NULL, \
    \requested_by TEXT NOT NULL, request_reference TEXT NOT NULL, \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (market_id, revision), UNIQUE (market_id, request_reference), \
    \CHECK (revision > 0), CHECK (requested_start_timestamp >= 0), \
    \CHECK (requested_by ~ '[^[:space:]]'), \
    \CHECK (request_reference ~ '[^[:space:]]'))"
  , "CREATE OR REPLACE FUNCTION protect_perps_candle_history_target() \
    \RETURNS TRIGGER LANGUAGE plpgsql AS $candle_history_target$ \
    \DECLARE current_revision BIGINT; BEGIN \
    \IF TG_OP <> 'INSERT' THEN \
    \ RAISE EXCEPTION 'candle history targets are immutable; append a revision' \
    \ USING ERRCODE = '55000'; \
    \END IF; \
    \PERFORM 1 FROM perps_candle_markets WHERE market_id = NEW.market_id FOR UPDATE; \
    \IF NOT FOUND THEN \
    \ RAISE EXCEPTION 'candle history target market does not exist' USING ERRCODE = '23503'; \
    \END IF; \
    \SELECT COALESCE(MAX(revision), 0) INTO current_revision \
    \FROM perps_candle_history_targets WHERE market_id = NEW.market_id; \
    \IF NEW.revision <> current_revision + 1 THEN \
    \ RAISE EXCEPTION 'candle history target must append the next revision' \
    \ USING ERRCODE = '23514'; \
    \END IF; NEW.created_at := NOW(); RETURN NEW; END $candle_history_target$"
  , "DO $candle_history_target_triggers$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger \
    \ WHERE tgname = 'perps_candle_history_target_identity' \
    \ AND tgrelid = 'perps_candle_history_targets'::regclass) THEN \
    \ CREATE TRIGGER perps_candle_history_target_identity BEFORE INSERT OR UPDATE OR DELETE \
    \ ON perps_candle_history_targets FOR EACH ROW \
    \ EXECUTE FUNCTION protect_perps_candle_history_target(); \
    \END IF; END $candle_history_target_triggers$"
  , "CREATE TABLE IF NOT EXISTS perps_market_release_epochs (\
    \market_id TEXT NOT NULL, release_revision BIGINT NOT NULL, \
    \chain_id BIGINT NOT NULL, release_router TEXT NOT NULL, \
    \cfd_engine TEXT NOT NULL, margin_clearinghouse TEXT NOT NULL, \
    \deployment_block BIGINT NOT NULL, deployment_block_hash TEXT NOT NULL, \
    \deployment_tx_hash TEXT NOT NULL, activation_block BIGINT NOT NULL, \
    \activation_timestamp BIGINT NOT NULL, activation_block_hash TEXT NOT NULL, \
    \approval_reference TEXT NOT NULL, \
    \is_market_genesis BOOLEAN NOT NULL DEFAULT FALSE, \
    \created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \PRIMARY KEY (market_id, release_revision), \
    \UNIQUE (market_id, activation_block), UNIQUE (chain_id, release_router), \
    \FOREIGN KEY (market_id, chain_id) REFERENCES perps_candle_markets(market_id, chain_id), \
    \CHECK (release_router ~ '^0x[0-9a-f]{40}$'), \
    \CHECK (cfd_engine ~ '^0x[0-9a-f]{40}$'), \
    \CHECK (margin_clearinghouse ~ '^0x[0-9a-f]{40}$'), \
    \CHECK (release_router <> '0x0000000000000000000000000000000000000000'), \
    \CHECK (cfd_engine <> '0x0000000000000000000000000000000000000000'), \
    \CHECK (margin_clearinghouse <> '0x0000000000000000000000000000000000000000'), \
    \CHECK (release_revision > 0), \
    \CHECK (is_market_genesis = (release_revision = 1)), \
    \CHECK (deployment_block > 0), CHECK (activation_block >= deployment_block), \
    \CHECK (activation_timestamp >= 0), \
    \CHECK (deployment_block_hash ~ '^0x[0-9a-f]{64}$'), \
    \CHECK (deployment_tx_hash ~ '^0x[0-9a-f]{64}$'), \
    \CHECK (activation_block_hash ~ '^0x[0-9a-f]{64}$'), \
    \CHECK (deployment_block_hash <> '0x0000000000000000000000000000000000000000000000000000000000000000'), \
    \CHECK (deployment_tx_hash <> '0x0000000000000000000000000000000000000000000000000000000000000000'), \
    \CHECK (activation_block_hash <> '0x0000000000000000000000000000000000000000000000000000000000000000'), \
    \CHECK (approval_reference ~ '[^[:space:]]'))"
  , "CREATE OR REPLACE FUNCTION protect_perps_market_release_identity() \
    \RETURNS TRIGGER LANGUAGE plpgsql AS $market_release_identity$ \
    \DECLARE market_chain_id BIGINT; current_revision BIGINT; \
    \latest_block BIGINT; latest_timestamp BIGINT; BEGIN \
    \IF TG_OP <> 'INSERT' THEN \
    \ RAISE EXCEPTION 'market release epochs are immutable; append a successor epoch' \
    \ USING ERRCODE = '55000'; \
    \END IF; \
    \SELECT chain_id INTO market_chain_id FROM perps_candle_markets \
    \WHERE market_id = NEW.market_id FOR UPDATE; \
    \IF NOT FOUND OR market_chain_id <> NEW.chain_id THEN \
    \ RAISE EXCEPTION 'market release does not match the logical market and chain' \
    \ USING ERRCODE = '23503'; \
    \END IF; \
    \SELECT release_revision, activation_block, activation_timestamp \
    \INTO current_revision, latest_block, latest_timestamp \
    \FROM perps_market_release_epochs WHERE market_id = NEW.market_id \
    \ORDER BY release_revision DESC LIMIT 1; \
    \IF NOT FOUND THEN \
    \ current_revision := 0; \
    \ELSE \
    \ IF NEW.activation_block <= latest_block \
    \  OR NEW.activation_timestamp < latest_timestamp THEN \
    \  RAISE EXCEPTION 'market release epochs must append in activation order' \
    \  USING ERRCODE = '23514'; \
    \ END IF; \
    \END IF; \
    \IF NEW.release_revision <> current_revision + 1 THEN \
    \ RAISE EXCEPTION 'market release must append the next revision' \
    \ USING ERRCODE = '23514'; \
    \END IF; NEW.created_at := NOW(); RETURN NEW; END $market_release_identity$"
  , "DO $market_release_triggers$ BEGIN \
    \IF NOT EXISTS (SELECT 1 FROM pg_trigger \
    \ WHERE tgname = 'perps_market_release_immutable' \
    \ AND tgrelid = 'perps_market_release_epochs'::regclass) THEN \
    \ CREATE TRIGGER perps_market_release_immutable BEFORE INSERT OR UPDATE OR DELETE \
    \ ON perps_market_release_epochs FOR EACH ROW \
    \ EXECUTE FUNCTION protect_perps_market_release_identity(); \
    \END IF; END $market_release_triggers$"
  ]
