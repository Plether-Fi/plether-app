import { execFileSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join, resolve } from 'node:path'
import { afterEach, describe, expect, it } from 'vitest'
import { input } from '../test/divisions'

const directories: string[] = []
afterEach(() => { for (const directory of directories.splice(0)) rmSync(directory, { recursive: true, force: true }) })

function directory() {
  const dir = mkdtempSync(join(tmpdir(), 'plether-divisions-'))
  directories.push(dir)
  return dir
}

function publish(dir: string, weekIndex: number, previous?: string, fixture = input(weekIndex)) {
  const path = join(dir, `input-${String(weekIndex)}.json`)
  writeFileSync(path, JSON.stringify(fixture))
  // Test-only clock lets the first real publication date remain in the future.
  const clock = `data:text/javascript,${encodeURIComponent("Date.now = () => Date.parse('2026-12-01T00:00:00Z')")}`
  return execFileSync(process.execPath, ['--import', clock, resolve('scripts/publish-divisions.mjs'), path, join(dir, 'publications'), ...(previous ? [previous] : [])], { encoding: 'utf8', stdio: 'pipe' })
}

describe('Snapshot publication CLI', () => {
  it('archives and serves weekly results, permits exact retries and rejects an altered published week', () => {
    const dir = directory()
    expect(publish(dir, 0)).toContain('1 traders')
    const current = join(dir, 'publications/current.json')
    const archived = join(dir, 'publications/weeks/2026-10-11.json')
    const first = readFileSync(current, 'utf8')
    expect(readFileSync(archived, 'utf8')).toBe(first)
    expect(() => publish(dir, 0)).not.toThrow()
    const altered = input()
    altered.accounts[0].closingEquityAssets = '130000000000'
    expect(() => publish(dir, 0, undefined, altered)).toThrow(/refusing to rewrite history/)
    expect(readFileSync(current, 'utf8')).toBe(first)
    expect(() => publish(dir, 1, archived)).not.toThrow()
    expect(JSON.parse(readFileSync(current, 'utf8')).standings[0].tier).toBe('Silver')
    expect(() => publish(dir, 0)).toThrow(/refusing to overwrite/)
    expect(readFileSync(archived, 'utf8')).toBe(first)
  })

  it('rejects malformed source data before writing publication assets', () => {
    const dir = directory()
    const malformed = input()
    malformed.accounts[0].startingEquityAssets = '100.5'
    expect(() => publish(dir, 0, undefined, malformed)).toThrow(/Invalid weekly snapshot/)
  })
})
