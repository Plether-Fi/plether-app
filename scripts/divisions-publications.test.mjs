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
