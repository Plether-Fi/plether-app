// PSQL_BIN and PGDATABASE select a local test database. All tables are temporary.
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { test } from 'node:test'
const read = path => readFileSync(new URL(path, import.meta.url), 'utf8')
const worker = read('../apps/backend/src/Plether/Insights/DivisionsLive.hs')
const handlers = read('../apps/backend/src/Plether/Handlers/Insights.hs')
const sqlString = text => text.slice(1, -1).replace(/\\\s*\n\s*\\/g, '')
const ddl = sqlString(worker.match(/"CREATE TABLE IF NOT EXISTS divisions_publications[\s\S]*?\)"/)[0]).replace('CREATE TABLE IF NOT EXISTS', 'CREATE TEMP TABLE')
const query = sqlString(handlers.split('getDivisionsCurrentResponse pool cfg = do')[1].match(/"SELECT[\s\S]*?LIMIT 1"/)[0])
  .replace('p.chain_id=?', 'p.chain_id=421614').replace('p.release_router=?', "p.release_router='router'")
const insert = sqlString(worker.match(/"INSERT INTO divisions_publications[\s\S]*?DO NOTHING"/)[0])
const fixtures = JSON.parse(read('../apps/backend/test/fixtures/divisions-publications.json'))
const run = sql => {
  const result = spawnSync(process.env.PSQL_BIN ?? 'psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', process.env.PGDATABASE ?? 'postgres'], { input: sql, encoding: 'utf8' })
  assert.equal(result.status, 0, result.stderr)
  return result.stdout.trim().split('\n').filter(Boolean).map(line => JSON.parse(line.slice(line.indexOf('|') + 1)))
}
const write = (week, publication, chain=421614, router='router') => {
  const args=[chain, `'${router}'`, week, week+99, `'${JSON.stringify(publication)}'`]
  return insert.replace(/\?/g,()=>String(args.shift()))+';'
}
const historyHandler = handlers.split('getDivisionsHistoryResponse pool cfg week requestedRelease = do')[1]
const historySql = sqlString(historyHandler.match(/"WITH weeks AS[\s\S]*?LIMIT 1"/)[0])
const deploymentsSql = sqlString(historyHandler.match(/"SELECT release_router FROM[\s\S]*?NULLS LAST, release_router"/)[0])
const bind = (sql, args) => sql.replace(/\?/g, () => {
  const value = args.shift()
  return value === null ? 'NULL' : typeof value === 'string' ? `'${value.replaceAll("'", "''")}'` : String(value)
})
const history = (router, week = null) => `SELECT json_build_object('publication', publication::jsonb, 'older', older_week, 'newer', newer_week)
  FROM (${bind(historySql, [421614, router, week, week])}) AS page(block, publication, older_week, newer_week);`

test('old contract results remain selectable with independent cursors and immutable awards', () => {
  const older = fixtures[1].expected
  const published = fixtures[2].expected
  const empty = fixtures[0].expected
  const rows = run(`BEGIN;
    ${ddl};
    CREATE TEMP TABLE insights_divisions_links (trading_account text PRIMARY KEY, discord_username text);
    ${write(100, older, 421614, 'previous')}
    ${write(200, published, 421614, 'previous')}
    ${write(300, empty, 421614, 'previous')}
    ${write(200, empty)}
    ${write(250, empty)}
    ${write(400, empty, 1, 'previous')}
    ${history('previous', 200)}
    ${history('router', 200)}
    ${history('previous')}
    ${history('previous', 100)}
    ${history('router')}
    SELECT to_json(COUNT(*)) FROM (${bind(historySql, [421614, 'unknown', null, null])}) page;
    ROLLBACK;`)
  assert.deepEqual(rows[0], { publication: { ...published, standings: published.standings.map(row => ({ ...row, displayName: null })) }, older: 100, newer: 300 })
  assert.deepEqual(rows[1], { publication: empty, older: null, newer: 250 })
  assert.deepEqual(rows[2], { publication: empty, older: 200, newer: null })
  assert.equal(rows[3].older, null)
  assert.equal(rows[3].newer, 200)
  assert.equal(rows[4].older, 200)
  assert.equal(rows[4].newer, null)
  assert.equal(rows[5], 0)
})

test('deployment discovery includes the current unpublished release and excludes other chains', () => {
  const releases = bind(deploymentsSql, [421614, 'current', 'current'])
  const list = `SELECT COALESCE(json_agg(release_router), '[]'::json) FROM (${releases}) releases;`
  const rows = run(`BEGIN;
    ${ddl};
    ${list}
    ${write(100, fixtures[0].expected, 421614, 'previous')}
    ${write(200, fixtures[0].expected, 421614, 'previous')}
    ${write(300, fixtures[0].expected, 1, 'other-chain')}
    ${list}
    ${write(200, fixtures[0].expected, 421614, 'current')}
    ${list}
    ROLLBACK;`)
  assert.deepEqual(rows, [['current'], ['current', 'previous'], ['current', 'previous']])
})

test('legacy USDC publications expose asset fields without rewriting stored awards', () => {
  const canonical = fixtures[2].expected
  const legacy = { ...canonical, standings: canonical.standings.map(({ weeklyPnlAssets, adjustedEquityAssets, ...row }) => ({
    ...row, weeklyPnlUsdc: weeklyPnlAssets, adjustedEquityUsdc: adjustedEquityAssets,
  })) }
  const rows = run(`BEGIN;
    ${ddl};
    CREATE TEMP TABLE insights_divisions_links (trading_account text PRIMARY KEY, discord_username text);
    ${write(100, legacy, 421614, 'previous')}
    ${history('previous')}
    SELECT publication FROM divisions_publications;
    ROLLBACK;`)
  assert.deepEqual(rows[0].publication.standings, canonical.standings.map((row, i) => ({ ...legacy.standings[i], ...row, displayName: null })))
  assert.deepEqual(rows[1], legacy)
})

test('awards survive retries, return only the newest week for this release, and preserve all wallets with current Discord names', () => {
  const expected = fixtures[2].expected
  const rows = run(`BEGIN;
    ${ddl};
    CREATE TEMP TABLE insights_divisions_links (trading_account text PRIMARY KEY, discord_username text);
    INSERT INTO insights_divisions_links VALUES ('${expected.standings[0].address}', 'registered-trader');
    ${write(100,fixtures[1].expected)}
    ${write(200,expected)}
    ${write(200,fixtures[0].expected)}
    ${write(300,fixtures[0].expected,1)}
    ${write(300,fixtures[0].expected,421614,'other-release')}
    ${query};
    UPDATE insights_divisions_links SET discord_username='renamed';
    ${query};
    ${write(400,fixtures[0].expected)}
    ${query};
    ROLLBACK;`)
  assert.deepEqual(rows[0], { ...expected, standings:expected.standings.map((row,i)=>({...row,displayName:i===0?'registered-trader':null})) })
  assert.equal(rows[1].standings[0].displayName,'renamed')
  assert.deepEqual(rows[2].standings,[])
})
