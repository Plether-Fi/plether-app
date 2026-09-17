-- Run with psql ON_ERROR_STOP outside a transaction, before release 2 rollout.
SET lock_timeout='1s';
SET statement_timeout='10min';
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_insights_transfers_normalized_inbound
ON perps_usdc_transfers(chain_id,release_router,LOWER(token_address),LOWER(to_address),block_number,tx_index,log_index);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_insights_transfers_normalized_outbound
ON perps_usdc_transfers(chain_id,release_router,LOWER(token_address),LOWER(from_address),block_number,tx_index,log_index);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_insights_transfers_normalized_transaction
ON perps_usdc_transfers(chain_id,release_router,LOWER(tx_hash),LOWER(token_address),block_number);
