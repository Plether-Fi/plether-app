-- Breaking schema rename for the USDG perps application.
-- Apply explicitly during an offline upgrade, after taking a database backup.
-- This file is deliberately NOT executed by API/worker startup.
-- It changes identifiers only: no balances, token addresses, signed bytes,
-- historical event payloads, receipts, deployment identities, or prize values.
BEGIN;
DO $$
DECLARE
  item record;
  new_name text;
BEGIN
  IF to_regclass('perps_usdc_transfers') IS NOT NULL THEN
    IF to_regclass('perps_asset_transfers') IS NOT NULL THEN
      RAISE EXCEPTION 'Both legacy and current asset transfer tables exist; reconcile before migration';
    END IF;
    ALTER TABLE perps_usdc_transfers RENAME TO perps_asset_transfers;
  END IF;
  FOR item IN
    SELECT c.table_name, c.column_name
      FROM information_schema.columns c
     WHERE c.table_schema = current_schema()
       AND c.table_name IN ('perps_orders', 'perps_account_activity', 'insights_competitions',
         'insights_account_snapshots', 'insights_manual_adjustments', 'insights_finalized_standings', 'aa_close_assistance')
       AND (c.column_name ~ '_usdc$' OR c.column_name = 'usdc_address')
  LOOP
    new_name := CASE WHEN item.column_name = 'usdc_address' THEN 'settlement_token_address'
                     ELSE regexp_replace(item.column_name, '_usdc$', '_assets') END;
    IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = current_schema()
      AND table_name = item.table_name AND column_name = new_name) THEN
      RAISE EXCEPTION 'Both legacy and current columns exist on %.%', item.table_name, new_name;
    END IF;
    EXECUTE format('ALTER TABLE %I RENAME COLUMN %I TO %I', item.table_name, item.column_name, new_name);
  END LOOP;
  FOR item IN
    SELECT conname FROM pg_constraint WHERE conrelid = to_regclass('perps_asset_transfers')
      AND conname LIKE '%usdc%'
  LOOP
    EXECUTE format('ALTER TABLE perps_asset_transfers RENAME CONSTRAINT %I TO %I',
      item.conname, replace(item.conname, 'usdc', 'asset'));
  END LOOP;
  FOR item IN
    SELECT indexname FROM pg_indexes WHERE schemaname = current_schema()
      AND tablename = 'perps_asset_transfers' AND indexname LIKE '%usdc%'
  LOOP
    EXECUTE format('ALTER INDEX %I RENAME TO %I', item.indexname, replace(item.indexname, 'usdc', 'asset'));
  END LOOP;
END $$;

-- PostgreSQL does not rewrite PL/pgSQL field references when columns rename.
CREATE OR REPLACE FUNCTION insights_integrity_metadata_epoch() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF ROW(NEW.chain_id, NEW.release_router, NEW.release_manifest, NEW.account_lens_address,
         NEW.settlement_token_address, NEW.margin_clearinghouse_address, NEW.start_block, NEW.start_block_hash,
         NEW.start_snapshot_block_hash, NEW.score_cutoff_block, NEW.score_cutoff_block_hash,
         NEW.start_timestamp, NEW.score_cutoff_timestamp, NEW.starting_balance_assets, NEW.registration_close_timestamp)
     IS DISTINCT FROM
     ROW(OLD.chain_id, OLD.release_router, OLD.release_manifest, OLD.account_lens_address,
         OLD.settlement_token_address, OLD.margin_clearinghouse_address, OLD.start_block, OLD.start_block_hash,
         OLD.start_snapshot_block_hash, OLD.score_cutoff_block, OLD.score_cutoff_block_hash,
         OLD.start_timestamp, OLD.score_cutoff_timestamp, OLD.starting_balance_assets, OLD.registration_close_timestamp) THEN
    NEW.integrity_input_epoch := OLD.integrity_input_epoch + 1;
  END IF;
  RETURN NEW;
END $$;
COMMIT;
