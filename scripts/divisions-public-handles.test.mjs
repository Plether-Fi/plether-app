// Run against a local PostgreSQL database with PSQL_BIN and PGDATABASE set.
// Fixtures use temporary tables; no persistent data is changed.
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { test } from 'node:test'

const handlers = readFileSync(new URL('../apps/backend/src/Plether/Handlers/Insights.hs', import.meta.url), 'utf8')
const registration = readFileSync(new URL('../apps/backend/src/Plether/Handlers/DivisionsRegistration.hs', import.meta.url), 'utf8')
const query = handlers.split('getDivisionsLiveResponse pool cfg = do')[1]
  .match(/"SELECT[\s\S]*?LIMIT 1"/)[0].slice(1, -1).replace(/\\\s*\n\s*\\/g, '')
  .replace('s.chain_id=?', 's.chain_id=421614')
  .replace('s.release_router=?', "s.release_router='router'")
  .replace('s.week_start=?', 's.week_start=100')
const createLinks = registration.match(/"CREATE TABLE IF NOT EXISTS insights_divisions_links .*"/)[0]
  .slice(1, -1).replace('CREATE TABLE IF NOT EXISTS', 'CREATE TEMP TABLE')
const address = n => `0x${n.toString(16).padStart(40, '0')}`
const standings = [1, 2, 3, 4].map(n => ({ address: address(n), rank: n, activeDays: 2, weeklyReturnBps: 100 - n }))

for (const legacySchema of [false, true]) test(`completed registrations show handles with ${legacySchema ? 'legacy' : 'fresh'} schema without changing standings`, () => {
  const sql = `BEGIN;
    ${createLinks};
    INSERT INTO insights_divisions_links
      (discord_id,discord_username,owner_wallet,trading_account,rules_version,privacy_version)
      VALUES ('10000000000000001','existing-trader','${address(11)}','${address(1)}','v1','v1'),
             ('10000000000000002','new-trader','${address(12)}','${address(2)}','v1','v1'),
             ('10000000000000003','owner-only','${address(4)}','${address(14)}','v1','v1');
    -- Simulate legacy records from the previous deployment. The old flag must
    -- not hide a completed registration or require a data backfill.
    ${legacySchema ? 'ALTER TABLE insights_divisions_links ADD COLUMN show_discord_handle BOOLEAN NOT NULL DEFAULT FALSE;' : ''}
    CREATE TEMP TABLE divisions_live_snapshots (
      chain_id bigint, release_router text, week_start bigint, snapshot_block bigint,
      snapshot_hash text, snapshot_timestamp bigint, wallet_count int, standings jsonb, captured_at timestamptz);
    CREATE TEMP TABLE perps_indexer_state (
      chain_id bigint, release_router text, indexer_name text, last_indexed_block bigint, last_indexed_block_hash text);
    INSERT INTO divisions_live_snapshots VALUES
      (421614,'router',100,123,'hash',100,4,'${JSON.stringify(standings)}',NOW());
    INSERT INTO perps_indexer_state VALUES (421614,'router','perps-history-costs-v2:finalized-abi3:router',123,'hash');
    ${query};
    UPDATE insights_divisions_links SET discord_username='renamed' WHERE trading_account='${address(2)}';
    ${query};
    UPDATE divisions_live_snapshots SET standings='[]',wallet_count=0;
    ${query};
    ROLLBACK;`
  const result = spawnSync(process.env.PSQL_BIN ?? 'psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', process.env.PGDATABASE ?? 'postgres'], { input: sql, encoding: 'utf8' })
  assert.equal(result.status, 0, result.stderr)
  const snapshots = result.stdout.trim().split('\n').map(line => JSON.parse(line.split('|')[4]))
  assert.deepEqual(snapshots[0], standings.map(row => ({ ...row, discordUsername: row.address === address(1) ? 'existing-trader' : row.address === address(2) ? 'new-trader' : null })))
  assert.equal(snapshots[1][1].discordUsername, 'renamed')
  assert.deepEqual(snapshots[2], [])
})
