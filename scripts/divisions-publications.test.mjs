// PSQL_BIN and PGDATABASE select a local test database. All tables are temporary.
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { test } from 'node:test'
const read = path => readFileSync(new URL(path, import.meta.url), 'utf8')
const worker = read('../apps/backend/src/Plether/Insights/DivisionsLive.hs')
const sqlString = text => text.slice(1, -1).replace(/\\\s*\n\s*\\/g, '')
const table = name => sqlString(worker.match(new RegExp(`"CREATE TABLE IF NOT EXISTS ${name}[\\s\\S]*?\\)"`))[0]).replace('CREATE TABLE IF NOT EXISTS', 'CREATE TEMP TABLE')
const query = sqlString(worker.split('divisionsPublicationsQuery =')[1].match(/"WITH weeks AS[\s\S]*?LIMIT 1"/)[0])
const insert = sqlString(worker.match(/"INSERT INTO divisions_publications[\s\S]*?DO NOTHING"/)[0])
const fixtures = JSON.parse(read('../apps/backend/test/fixtures/divisions-publications.json'))
const start = 1791752400
const old = { router: '0x9b9ddc783780a14e4930dbbcd94d6356a82725e8', version: 'v1.3.1', assetSymbol: 'USDC' }
const current = { router: '0x918663d6da8cb33b325a04c4a5e8c51d42f3b444', version: 'v2.0.0', assetSymbol: 'USDG' }
const bind = (sql, args) => sql.replace(/\?/g, () => {
  const value = args.shift()
  return value === null ? 'NULL' : typeof value === 'string' ? `'${value.replaceAll("'", "''")}'` : String(value)
})
const run = sql => {
  const result = spawnSync(process.env.PSQL_BIN ?? 'psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', process.env.PGDATABASE ?? 'postgres'], { input: sql, encoding: 'utf8' })
  assert.equal(result.status, 0, result.stderr)
  return result.stdout.trim().split('\n').filter(Boolean).map(line => JSON.parse(line.slice(line.indexOf('|') + 1)))
}
const write = (week, publication, release=old, chain=421614) => bind(insert, [chain, release.router, week, week+99, JSON.stringify(publication)])+';'
const pin = (week, release=old) => `INSERT INTO divisions_weeks VALUES (421614,${week},'${release.router}','${release.version}','${release.assetSymbol}') ON CONFLICT DO NOTHING;`
const history = (week=null) => `SELECT json_build_object('publication', publication::jsonb, 'older', older_week, 'newer', newer_week)
  FROM (${bind(query, [421614, week, week])}) AS page(block, publication, older_week, newer_week);`
const setup = `BEGIN; ${table('divisions_publications')}; ${table('divisions_weeks')};
  CREATE TEMP TABLE insights_divisions_links (trading_account text PRIMARY KEY, discord_username text);`
const normalized = (publication, release=old) => ({ ...publication, scoringRelease: release,
  standings: publication.standings.map(row => ({ ...row, displayName: null })) })

test('one pinned contract counts per week; history crosses upgrades and preserves awarded badges', () => {
  const older = fixtures[1].expected
  const published = fixtures[2].expected
  const empty = fixtures[0].expected
  const week = start+2*604800
  const rows = run(`${setup}
    ${pin(start)} ${pin(start+604800)} ${pin(week)} ${pin(week,current)}
    ${write(start,empty)} ${write(start+604800,older)} ${write(week,published)}
    ${write(week,empty,current)} ${write(week+604800,empty,current,1)}
    ${history()}
    SELECT to_json(COUNT(*)) FROM divisions_weeks WHERE week_start=${week};
    ${pin(week+604800,current)} ${write(week+604800,empty,current)}
    ${history(week)} ${history()} ${history(start)}
    ROLLBACK;`)
  assert.deepEqual(rows[0], { publication: normalized(published), older: start+604800, newer: null })
  assert.equal(rows[1], 1)
  assert.deepEqual(rows[2], { publication: normalized(published), older: start+604800, newer: week+604800 })
  assert.deepEqual(rows[3], { publication: normalized(empty,current), older: week, newer: null })
  assert.equal(rows[4].older,null)
  assert.equal(rows[4].newer,start+604800)
})

test('legacy USDC awards normalize without rewriting saved publications', () => {
  const canonical = fixtures[2].expected
  const legacy = { ...canonical, standings: canonical.standings.map(({ weeklyPnlAssets, adjustedEquityAssets, ...row }) => ({
    ...row, weeklyPnlUsdc: weeklyPnlAssets, adjustedEquityUsdc: adjustedEquityAssets,
  })) }
  const rows = run(`${setup} ${pin(start)} ${write(start,legacy)} ${history()}
    SELECT publication FROM divisions_publications; ROLLBACK;`)
  assert.deepEqual(rows[0].publication.standings, canonical.standings.map((row,i)=>({...legacy.standings[i],...row,displayName:null})))
  assert.deepEqual(rows[0].publication.scoringRelease,old)
  assert.deepEqual(rows[1],legacy)
})

test('retries cannot overwrite awards, and public handles refresh independently of contract version', () => {
  const expected=fixtures[2].expected
  const rows=run(`${setup} ${pin(start)}
    INSERT INTO insights_divisions_links VALUES ('${expected.standings[0].address}','registered-trader');
    ${write(start,expected)} ${write(start,fixtures[0].expected)} ${history()}
    UPDATE insights_divisions_links SET discord_username='renamed'; ${history()}
    SELECT to_json(COUNT(*)) FROM (${bind(query,[421614,start+604800,start+604800])}) page;
    ROLLBACK;`)
  assert.deepEqual(rows[0].publication,{...normalized(expected),standings:expected.standings.map((row,i)=>({...row,displayName:i===0?'registered-trader':null}))})
  assert.equal(rows[1].publication.standings[0].displayName,'renamed')
  assert.equal(rows[2],0)
})
