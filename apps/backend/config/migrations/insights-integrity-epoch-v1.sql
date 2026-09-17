-- Additive metadata; safe to apply before either API or worker is upgraded.
ALTER TABLE insights_competitions
  ADD COLUMN IF NOT EXISTS integrity_input_epoch bigint NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS integrity_checked_epoch bigint,
  ADD COLUMN IF NOT EXISTS integrity_checked_at timestamptz,
  ADD COLUMN IF NOT EXISTS integrity_as_of_block bigint,
  ADD COLUMN IF NOT EXISTS integrity_as_of_hash text,
  ADD COLUMN IF NOT EXISTS integrity_max_age_seconds integer NOT NULL DEFAULT 120;

CREATE OR REPLACE FUNCTION insights_integrity_metadata_epoch() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF ROW(NEW.chain_id, NEW.release_router, NEW.release_manifest, NEW.account_lens_address,
         NEW.usdc_address, NEW.margin_clearinghouse_address, NEW.start_block, NEW.start_block_hash,
         NEW.start_snapshot_block_hash, NEW.score_cutoff_block, NEW.score_cutoff_block_hash,
         NEW.start_timestamp, NEW.score_cutoff_timestamp, NEW.starting_balance_usdc, NEW.registration_close_timestamp)
     IS DISTINCT FROM
     ROW(OLD.chain_id, OLD.release_router, OLD.release_manifest, OLD.account_lens_address,
         OLD.usdc_address, OLD.margin_clearinghouse_address, OLD.start_block, OLD.start_block_hash,
         OLD.start_snapshot_block_hash, OLD.score_cutoff_block, OLD.score_cutoff_block_hash,
         OLD.start_timestamp, OLD.score_cutoff_timestamp, OLD.starting_balance_usdc, OLD.registration_close_timestamp) THEN
    NEW.integrity_input_epoch := OLD.integrity_input_epoch + 1;
  END IF;
  RETURN NEW;
END $$;
CREATE OR REPLACE TRIGGER insights_integrity_metadata_epoch
BEFORE UPDATE ON insights_competitions FOR EACH ROW EXECUTE FUNCTION insights_integrity_metadata_epoch();

CREATE OR REPLACE FUNCTION insights_integrity_roster_epoch() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND ROW(NEW.competition_slug,NEW.wallet,NEW.trader_reference)
       IS NOT DISTINCT FROM ROW(OLD.competition_slug,OLD.wallet,OLD.trader_reference) THEN
    RETURN NULL;
  END IF;
  IF TG_OP <> 'INSERT' THEN
    UPDATE insights_competitions SET integrity_input_epoch=integrity_input_epoch+1
      WHERE slug=OLD.competition_slug AND NOT finalized;
  END IF;
  IF TG_OP = 'INSERT' OR (TG_OP = 'UPDATE' AND NEW.competition_slug <> OLD.competition_slug) THEN
    UPDATE insights_competitions SET integrity_input_epoch=integrity_input_epoch+1
      WHERE slug=NEW.competition_slug AND NOT finalized;
  END IF;
  RETURN NULL;
END $$;
CREATE OR REPLACE TRIGGER insights_integrity_roster_epoch
AFTER INSERT OR UPDATE OF competition_slug,wallet,trader_reference OR DELETE ON insights_competition_participants
FOR EACH ROW EXECUTE FUNCTION insights_integrity_roster_epoch();

CREATE OR REPLACE FUNCTION insights_integrity_baseline_epoch() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP='UPDATE' AND ROW(NEW.competition_slug,NEW.snapshot_kind,NEW.block_number,NEW.block_hash,NEW.participant_count,NEW.published_at)
      IS NOT DISTINCT FROM ROW(OLD.competition_slug,OLD.snapshot_kind,OLD.block_number,OLD.block_hash,OLD.participant_count,OLD.published_at) THEN
    RETURN NULL;
  END IF;
  IF TG_OP <> 'INSERT' AND OLD.snapshot_kind='start' THEN
    UPDATE insights_competitions SET integrity_input_epoch=integrity_input_epoch+1
      WHERE slug=OLD.competition_slug AND NOT finalized;
  END IF;
  IF TG_OP <> 'DELETE' AND NEW.snapshot_kind='start' THEN
    UPDATE insights_competitions SET integrity_input_epoch=integrity_input_epoch+1
      WHERE slug=NEW.competition_slug AND NOT finalized;
  END IF;
  RETURN NULL;
END $$;
CREATE OR REPLACE TRIGGER insights_integrity_baseline_epoch
AFTER INSERT OR UPDATE OR DELETE ON insights_snapshot_batches
FOR EACH ROW EXECUTE FUNCTION insights_integrity_baseline_epoch();

-- Invalidate inside the same transaction that removes canonical history,
-- even when the rewind does not touch an existing snapshot batch.
CREATE OR REPLACE FUNCTION insights_integrity_history_epoch() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE insights_competitions c SET integrity_input_epoch=integrity_input_epoch+1
    WHERE NOT c.finalized AND EXISTS
      (SELECT 1 FROM removed_history h WHERE h.chain_id=c.chain_id AND h.release_router=c.release_router);
  RETURN NULL;
END $$;
DROP TRIGGER IF EXISTS insights_integrity_history_epoch ON perps_account_activity;
CREATE TRIGGER insights_integrity_history_epoch AFTER DELETE ON perps_account_activity
REFERENCING OLD TABLE AS removed_history FOR EACH STATEMENT EXECUTE FUNCTION insights_integrity_history_epoch();
DROP TRIGGER IF EXISTS insights_integrity_history_epoch ON perps_usdc_transfers;
CREATE TRIGGER insights_integrity_history_epoch AFTER DELETE ON perps_usdc_transfers
REFERENCING OLD TABLE AS removed_history FOR EACH STATEMENT EXECUTE FUNCTION insights_integrity_history_epoch();

CREATE OR REPLACE FUNCTION insights_integrity_cursor_epoch() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.last_indexed_block < OLD.last_indexed_block
    OR (NEW.last_indexed_block = OLD.last_indexed_block AND NEW.last_indexed_block_hash IS DISTINCT FROM OLD.last_indexed_block_hash)
    OR NEW.configured_start_block IS DISTINCT FROM OLD.configured_start_block THEN
    UPDATE insights_competitions SET integrity_input_epoch=integrity_input_epoch+1
      WHERE chain_id=NEW.chain_id AND release_router=NEW.release_router AND NOT finalized;
  END IF;
  RETURN NULL;
END $$;
CREATE OR REPLACE TRIGGER insights_integrity_cursor_epoch AFTER UPDATE ON perps_indexer_state
FOR EACH ROW EXECUTE FUNCTION insights_integrity_cursor_epoch();
