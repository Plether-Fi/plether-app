import assert from 'node:assert/strict'
import { readFile, mkdtemp, mkdir, writeFile, symlink, rm } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { spawnSync } from 'node:child_process'
import { test } from 'node:test'

const script = await readFile(new URL('./reproduce-close-review.mjs', import.meta.url), 'utf8')
const historical = JSON.parse(await readFile(new URL('../../../config/perps/arbitrum-sepolia-v2.json', import.meta.url), 'utf8'))
const current = JSON.parse(await readFile(new URL('../../../config/perps/arbitrum-sepolia-v3.json', import.meta.url), 'utf8'))

async function startup(release, withBundle = false) {
  const root = await mkdtemp(path.join(tmpdir(), 'plether-close-reproduction-'))
  try {
    const frontend = path.join(root, 'apps/frontend')
    await mkdir(path.join(frontend, 'scripts'), { recursive: true })
    await mkdir(path.join(root, 'config/perps'), { recursive: true })
    await writeFile(path.join(root, 'config/perps/arbitrum-sepolia-v2.json'), JSON.stringify(release))
    // Also provide the current manifest: changing the diagnostic back to the
    // moving deployment must be caught instead of passing on an absent file.
    await writeFile(path.join(root, 'config/perps/arbitrum-sepolia-v3.json'), JSON.stringify(current))
    await writeFile(path.join(frontend, 'scripts/reproduce-close-review.mjs'), script)
    await symlink(fileURLToPath(new URL('../node_modules', import.meta.url)), path.join(frontend, 'node_modules'), 'dir')
    const bundle = path.join(root, 'wrong-bundle.tar.gz')
    await writeFile(bundle, 'not the pinned release bundle')
    const result = spawnSync(process.execPath, [path.join(frontend, 'scripts/reproduce-close-review.mjs'), ...(withBundle ? [bundle] : [])], {
      encoding: 'utf8', timeout: 10_000,
      // All cases must fail before an RPC client is used.
      env: { ...process.env, HTTP_PROXY: 'http://127.0.0.1:1', HTTPS_PROXY: 'http://127.0.0.1:1' },
    })
    assert.equal(result.error, undefined)
    assert.notEqual(result.status, 0)
    return result.stderr
  } finally {
    await rm(root, { recursive: true, force: true })
  }
}

test('requires the incident release bundle and names its historical block', async () => {
  assert.match(await startup(historical), /v1\.2\.3 ABI bundle.*historical block 308891788/)
})

test('rejects the later v1.3.1 deployment before bundle loading or RPC', async () => {
  assert.match(await startup(current), /Unsupported historical reproduction release/)
})

test('rejects a relabeled source or bundle identity before RPC', async () => {
  assert.match(await startup({ ...historical, release: { ...historical.release, sourceCommit: current.release.sourceCommit } }), /Unsupported historical reproduction source/)
  assert.match(await startup({ ...historical, release: { ...historical.release, bundleSha256: current.release.bundleSha256 } }), /Unsupported historical reproduction bundle/)
})

test('rejects a missing deployment block or a deployment after the incident', async () => {
  assert.match(await startup({ ...historical, release: { ...historical.release, deploymentBlock: null } }), /Historical release requires its deployment block/)
  assert.match(await startup({ ...historical, release: { ...historical.release, deploymentBlock: 318398363 } }), /Historical reproduction block predates the selected deployment/)
})

test('rejects a bundle with the wrong checksum before reading its ABI or RPC', async () => {
  assert.match(await startup(historical, true), /ABI bundle checksum mismatch/)
})
