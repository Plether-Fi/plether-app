import assert from 'node:assert/strict'
import { execFileSync, spawn } from 'node:child_process'
import { mkdtempSync, readFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import net from 'node:net'
import { once } from 'node:events'

// Creates its own loopback-only chain. No RPC URL, fork, .env, private key,
// deployment script, broadcast, or registry publication is accepted.
const root = fileURLToPath(new URL('..', import.meta.url))
const frontend = path.join(root, 'apps/frontend')
const backend = path.join(root, 'apps/backend')
assert.ok(process.argv[2], 'Usage: node scripts/test-order-v3-local.mjs /path/to/plether-core')
const core = path.resolve(process.argv[2])
const release = JSON.parse(readFileSync(path.join(frontend, 'vendor/perps-aa-client/release.json')))
const git = (...args) => execFileSync('git', ['-C', core, ...args], { encoding: 'utf8' }).trim()
assert.equal(git('rev-parse', 'HEAD'), release.gitHead, 'Use the Core commit recorded in the SDK artifact')
assert.equal(git('status', '--porcelain', '--untracked-files=no'), '', 'Core sources must match the pinned commit')
const env = Object.fromEntries(Object.entries(process.env).filter(([key]) => !/^(FOUNDRY_|DAPP_|ETH_RPC_URL$|RPC_URL$|PLETHER_LOCAL_)/.test(key)))
const run = (cmd, args, cwd) => execFileSync(cmd, args, { cwd, env, stdio: 'inherit' })
const work = mkdtempSync(path.join(tmpdir(), 'plether-v3-local-'))
run(process.execPath, ['scripts/verify-vendored-perps-aa-client.mjs'], root)
run('forge', ['build', '--root', 'packages/perps', '--skip', 'test', '--skip', 'script', 'packages/shared/test-support/MockUSDC.sol', 'packages/shared/test-support/MockPyth.sol'], core)
run('forge', ['build', '--root', 'packages/perps-aa', '--skip', 'test', '--skip', 'script'], core)
run('cabal', ['build', 'lib:plether-api'], backend)
const policy = path.join(work, 'order-v3-policy')
run('cabal', ['exec', '--', 'ghc', '-package', 'plether-api', 'test-local/OrderV3Policy.hs', '-outputdir', work, '-o', policy], backend)
run('npm', ['ci', '--ignore-scripts', '--no-audit', '--no-fund'], path.join(frontend, 'test/local-order-v3'))
const server = net.createServer()
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
const port = server.address().port
await new Promise(resolve => server.close(resolve))
const child = spawn('anvil', ['--host', '127.0.0.1', '--port', String(port), '--chain-id', '421614', '--timestamp', '1709532000', '--base-fee', '0', '--gas-limit', '100000000', '--silent'], { env, stdio: 'ignore' })
let failed
child.on('error', error => { failed = error })
child.on('exit', code => { failed = new Error(`Local Anvil exited: ${code}`) })
const rpc = `http://127.0.0.1:${port}`
try {
  for (let i = 0; ; i++) {
    if (failed) throw failed
    try {
      const response = await fetch(rpc, { method: 'POST', body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'web3_clientVersion', params: [] }), headers: { 'Content-Type': 'application/json' } })
      assert.match((await response.json()).result, /anvil/i)
      break
    } catch (error) { if (i >= 100) throw error }
    await new Promise(resolve => setTimeout(resolve, 50))
  }
  execFileSync(path.join(frontend, 'node_modules/.bin/vitest'), ['run', '--config', 'test/local-order-v3/vitest.config.ts'], {
    cwd: frontend, stdio: 'inherit', env: { ...env, PLETHER_LOCAL_RPC: rpc, PLETHER_CORE_PATH: core, PLETHER_LOCAL_POLICY_BIN: policy },
  })
} finally {
  if (child.exitCode === null) { const stopped = once(child, 'exit'); child.kill(); await stopped }
}
