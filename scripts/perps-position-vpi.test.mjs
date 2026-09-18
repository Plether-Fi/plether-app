// Run with a local PostgreSQL database: PSQL_BIN=/path/to/psql node --test scripts/perps-position-vpi.test.mjs
// All fixtures use session-local temporary tables and are rolled back.
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { test } from 'node:test'

const source = readFileSync(new URL('../apps/backend/src/Plether/Database/Schema.hs', import.meta.url), 'utf8')
const query = source.split('perpsOrderBaseSelectSql =\n')[1].split('\n\ngetPerpsOrdersByAccount')[0]
  .trim().slice(1, -1).replace(/\\\s*\n\s*\\/g, '')
  .replace('o.chain_id = ?', 'o.chain_id = 1').replace('o.order_router = ?', "o.order_router = 'router'")

// postgresql-simple treats question marks as parameters; SQL regexes must not add placeholders.
assert.equal((query.match(/\?/g) ?? []).length, 0)

function fixture(steps, { account = 'account', router = 'router', block = 1, tx = 'tx' } = {}) {
  let size = 0
  return steps.map(({ kind, quantity, vpi, postSize, receipt = true, economics: extraEconomics = {} }, index) => {
    size += kind === 'Open' ? quantity : -quantity
    const id = block * 1000 + index
    const log = index * 10
    const economics = JSON.stringify({
      ...(vpi === undefined ? {} : { vpiUsdc: String(vpi) }),
      postPositionSize: String(postSize ?? size), executionBountyUsdc: '0', ...extraEconomics,
    })
    return `
      INSERT INTO perps_account_activity VALUES
        (1, '${router}', '${account}', '${kind}', 0, 100, ${quantity}, 0, '${tx}', ${block}, ${log}, '{}', NULL);
      ${receipt ? `INSERT INTO perps_events VALUES
        (1, '${router}', '${account}', '${tx}', ${block}, 'hash', 0, ${log + 1}, ${id}, 'OrderFinalized',
          '{"economics":${economics}}');` : ''}
      INSERT INTO perps_orders (chain_id, order_router, account, side, order_id, terminal_tx_hash,
        terminal_block_number, terminal_status, client_order_id, receipt_economics)
      VALUES (1, '${router}', '${account}', 0, ${id}, '${tx}', ${block}, 'Executed', 'client', '${economics}');`
  }).join('\n')
}

const setup = `BEGIN;
CREATE TEMP TABLE perps_orders (
  chain_id bigint, order_router text, account text, side integer, order_id bigint,
  commit_tx_hash text, commit_block_number bigint, commit_timestamp bigint,
  terminal_tx_hash text, terminal_block_number bigint, terminal_timestamp bigint, terminal_status text,
  failure_reason text, execution_price numeric, execution_vpi_usdc numeric,
  execution_frozen_close_spread_usdc numeric, execution_economics_version integer,
  execution_oracle_price numeric, execution_oracle_frozen boolean, oracle_min_publish_time bigint,
  oracle_max_publish_time bigint, oracle_derivation_version integer, client_order_id text,
  receipt_hash text, terminal_reason text, pending_reason text, execution_mode text, failed_constraint text,
  receipt_economics jsonb, cleanup_actor text
);
CREATE TEMP TABLE perps_account_activity (
  chain_id bigint, release_router text, account text, activity_type text, side integer, price numeric,
  size_delta numeric, pnl_usdc numeric, tx_hash text, block_number bigint, log_index bigint, data jsonb, amount_usdc numeric
);
CREATE TEMP TABLE perps_events (
  chain_id bigint, release_router text, account text, tx_hash text, block_number bigint,
  block_hash text, tx_index bigint, log_index bigint, order_id bigint, event_name text, data jsonb
);`

const cases = [
  {
    name: 'full close includes the opening charge and exact close rebate',
    steps: [{ kind: 'Open', quantity: 100, vpi: 400 }, { kind: 'Close', quantity: 100, vpi: -250 }],
    expected: '150',
  },
  {
    name: 'increase and partial close retain the lifetime balance',
    steps: [{ kind: 'Open', quantity: 100, vpi: -300 }, { kind: 'Open', quantity: 50, vpi: 20 }, { kind: 'Close', quantity: 75, vpi: 100 }],
    expected: '-180',
  },
  {
    name: 'reopening resets the lifetime even in the same transaction',
    steps: [{ kind: 'Open', quantity: 100, vpi: 400 }, { kind: 'Close', quantity: 100, vpi: -250 }, { kind: 'Open', quantity: 200, vpi: 20 }, { kind: 'Close', quantity: 200, vpi: -20 }],
    expected: '0',
  },
  {
    name: 'an opening beyond the history page is included',
    steps: [{ kind: 'Open', quantity: 100, vpi: 400 }, ...Array.from({ length: 35 }, () => ({ kind: 'Close', quantity: 1, vpi: -1 }))],
    expected: '365',
  },
  {
    name: 'missing opening history is unavailable',
    steps: [{ kind: 'Close', quantity: 100, vpi: -250, postSize: 0 }], expected: null,
  },
  {
    name: 'missing finalized VPI cannot be replaced with activity estimates',
    steps: [{ kind: 'Open', quantity: 100 }, { kind: 'Close', quantity: 100, vpi: -250 }], expected: null,
  },
  {
    name: 'missing receipt is unavailable',
    steps: [{ kind: 'Open', quantity: 100, vpi: 400, receipt: false }, { kind: 'Close', quantity: 100, vpi: -250 }], expected: null,
  },
  {
    name: 'inconsistent remaining sizes are unavailable',
    steps: [{ kind: 'Open', quantity: 100, vpi: 400, postSize: 200 }, { kind: 'Close', quantity: 100, vpi: -250 }], expected: null,
  },
  {
    name: 'a previous liquidation does not enter a reopened position total',
    steps: [{ kind: 'Open', quantity: 100, vpi: 400 }, { kind: 'Liquidated', quantity: 100, receipt: false }, { kind: 'Open', quantity: 200, vpi: 20 }, { kind: 'Close', quantity: 200, vpi: -10 }], expected: '10',
  },
]

for (const { name, steps, expected } of cases) {
  test(name, () => {
    const noise = [{ kind: 'Open', quantity: 100, vpi: 999 }, { kind: 'Close', quantity: 100, vpi: 999 }]
    const sql = setup + fixture(steps)
      + fixture(noise, { account: 'other', block: 2 })
      + fixture(noise, { router: 'other', block: 3 })
      + `UPDATE perps_account_activity SET data = '{"vpiUsdc":"999999"}';
        SELECT jsonb_build_object('total', receipt_economics->>'totalPositionVpiUsdc',
          'lifetime', receipt_economics->>'positionLifetimeNetResultUsdc')
        FROM (${query} AND o.account = 'account' ORDER BY o.order_id DESC LIMIT 1) result;
        ROLLBACK;`
    const result = spawnSync(process.env.PSQL_BIN ?? 'psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', process.env.PGDATABASE ?? 'postgres'], { input: sql, encoding: 'utf8' })
    assert.equal(result.status, 0, result.stderr || result.error?.message)
    assert.deepEqual(JSON.parse(result.stdout.trim()), { total: expected, lifetime: null })
  })
}

function balances(before, after, claimBefore = 0, claimAfter = 0) {
  return {
    preSettlementBalanceUsdc: String(before), postSettlementBalanceUsdc: String(after),
    preTraderClaimBalanceUsdc: String(claimBefore), postTraderClaimBalanceUsdc: String(claimAfter),
  }
}

test('lifetime result separates execution costs, between-trade carry, and external funding', () => {
  const steps = [
    { kind: 'Open', quantity: 100, vpi: 5, economics: balances(1000, 990) },
    { kind: 'Open', quantity: 50, vpi: 2, economics: balances(1038, 1034) },
    { kind: 'Close', quantity: 75, vpi: -1, economics: balances(1024, 1044) },
  ]
  const sql = setup + fixture(steps) + `
    INSERT INTO perps_account_activity (chain_id, release_router, account, activity_type, block_number, log_index, amount_usdc)
    VALUES (1, 'router', 'account', 'Deposit', 1, 5, 50),
      (1, 'router', 'account', 'Withdraw', 1, 15, 10),
      (1, 'router', 'other', 'Deposit', 1, 6, 999),
      (1, 'other', 'account', 'Deposit', 1, 7, 999),
      (1, 'router', 'account', 'Deposit', 0, 0, 999),
      (1, 'router', 'account', 'Deposit', 2, 0, 999);
    SELECT jsonb_build_object('net', receipt_economics->>'positionLifetimeNetResultUsdc',
      'trades', receipt_economics->>'positionLifetimeTradesResultUsdc',
      'between', receipt_economics->>'positionLifetimeAccountAdjustmentUsdc')
    FROM (${query} ORDER BY o.order_id DESC LIMIT 1) result;
    ROLLBACK;`
  const result = spawnSync(process.env.PSQL_BIN ?? 'psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', process.env.PGDATABASE ?? 'postgres'], { input: sql, encoding: 'utf8' })
  assert.equal(result.status, 0, result.stderr || result.error?.message)
  assert.deepEqual(JSON.parse(result.stdout.trim()), { net: '4', trades: '6', between: '-2' })
})

test('claim settlement preserves lifetime result and a new position resets its baseline', () => {
  const steps = [
    { kind: 'Open', quantity: 100, vpi: 5, economics: balances(1000, 990) },
    { kind: 'Close', quantity: 100, vpi: 5, economics: balances(990, 1010) },
    { kind: 'Open', quantity: 100, vpi: 2, economics: balances(1010, 1005, 50, 50) },
    { kind: 'Close', quantity: 50, vpi: -1, economics: balances(1055, 1056) },
    { kind: 'Close', quantity: 50, vpi: -1, economics: balances(1056, 1050, 0, 10) },
  ]
  const sql = setup + fixture(steps) + `
    SELECT jsonb_build_object('net', receipt_economics->>'positionLifetimeNetResultUsdc',
      'trades', receipt_economics->>'positionLifetimeTradesResultUsdc',
      'between', receipt_economics->>'positionLifetimeAccountAdjustmentUsdc')
    FROM (${query} ORDER BY o.order_id DESC LIMIT 1) result;
    ROLLBACK;`
  const result = spawnSync(process.env.PSQL_BIN ?? 'psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', process.env.PGDATABASE ?? 'postgres'], { input: sql, encoding: 'utf8' })
  assert.equal(result.status, 0, result.stderr || result.error?.message)
  assert.deepEqual(JSON.parse(result.stdout.trim()), { net: '0', trades: '0', between: '0' })
})
