import assert from 'node:assert/strict'
import { mkdtempSync, readFileSync, statSync, readdirSync, rmSync, symlinkSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { test } from 'node:test'
import { pathToFileURL } from 'node:url'
import { isCliEntry, scrubRpcError, writePublicEvidence } from './local-safety.mjs'

test('publishes complete mode-0600 evidence and preserves an exclusive intent', () => {
  const folder = mkdtempSync(join(tmpdir(), 'kernel-qualified-evidence-'))
  try {
    const path = join(folder, 'evidence.json')
    writePublicEvidence(path, '{"status":"intent"}\n', true)
    assert.throws(() => writePublicEvidence(path, '{}\n', true), { code: 'EEXIST' })
    assert.deepEqual(JSON.parse(readFileSync(path, 'utf8')), { status: 'intent' })
    writePublicEvidence(path, '{"status":"included"}\n')
    assert.deepEqual(JSON.parse(readFileSync(path, 'utf8')), { status: 'included' })
    assert.equal(statSync(path).mode & 0o777, 0o600)
    assert.deepEqual(readdirSync(folder), ['evidence.json'])
  } finally {
    rmSync(folder, { recursive: true, force: true })
  }
})

test('resolves a CLI symlink and escapes spaces and special URL characters', () => {
  const folder = mkdtempSync(join(tmpdir(), 'kernel-qualified-cli-'))
  try {
    const target = join(folder, 'cli # check.mjs')
    const link = join(folder, 'tool.mjs')
    writeFileSync(target, '')
    symlinkSync(target, link)
    assert.equal(isCliEntry(pathToFileURL(target).href, link), true)
  } finally {
    rmSync(folder, { recursive: true, force: true })
  }
})

test('scrubs RPC URLs, bearer tokens and uppercase bare or prefixed deployer keys', () => {
  const bare = 'ab'.repeat(32)
  const bearer = 'canary-only-token'
  const error = new Error(`from 0X${bare.toUpperCase()} or ${bare} at https://example.invalid/vendor-token using ${bearer}`)
  assert.equal(scrubRpcError(error, [`0x${bare}`, bearer]), 'from [REDACTED] or [REDACTED] at [RPC] using [REDACTED]')
})
