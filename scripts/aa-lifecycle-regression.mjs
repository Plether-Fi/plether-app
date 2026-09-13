import assert from 'node:assert/strict'
import { execFileSync, spawnSync } from 'node:child_process'
import { copyFileSync, mkdtempSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { resolve, join } from 'node:path'
import { fileURLToPath } from 'node:url'

// Offline only: export immutable objects from an existing Core clone, never
// checkout/edit that clone, fetch an RPC, load .env, sign live, or deploy.
const core = process.argv[2]
assert.ok(core, 'Usage: node scripts/aa-lifecycle-regression.mjs /path/to/plether-core')
const source = 'ffe45937b7f38133133ad292c5435828bf99357d'
const git = (...args) => execFileSync('git', ['-C', resolve(core), ...args], { maxBuffer: 100 * 1024 * 1024 })
assert.equal(git('rev-parse', `${source}^{commit}`).toString().trim(), source)
const directory = mkdtempSync(join(tmpdir(), 'plether-aa-lifecycle-'))
execFileSync('tar', ['-x', '-C', directory], { input: git('archive', source) })
for (const name of ['forge-std', 'openzeppelin-contracts']) {
  const entry = git('ls-tree', source, `lib/${name}`).toString()
  const match = /^160000 commit ([0-9a-f]{40})\t/.exec(entry)
  assert.ok(match, `Missing pinned ${name} submodule`)
  const archive = execFileSync('git', ['-C', join(resolve(core), 'lib', name), 'archive', match[1]], { maxBuffer: 100 * 1024 * 1024 })
  execFileSync('tar', ['-x', '-C', join(directory, 'lib', name)], { input: archive })
}
const fixtures = fileURLToPath(new URL('./fixtures/', import.meta.url))
const cwd = join(directory, 'packages/perps')
copyFileSync(join(fixtures, 'aa-deposit-gas-20260912.json'), join(cwd, 'aa-fixture.json'))
copyFileSync(join(fixtures, 'NativeAALifecycle.t.sol'), join(cwd, 'test/perps/NativeAALifecycle.t.sol'))
copyFileSync(join(fixtures, 'aa-lifecycle-foundry.toml'), join(cwd, 'foundry.toml'))
console.log(`Core ${source}; isolated evidence directory: ${directory}`)
const result = spawnSync('forge', ['test', '--offline', '--match-path', 'test/perps/NativeAALifecycle.t.sol', '--match-test', 'test_NativeAA', '-vvv'], {
  cwd, stdio: 'inherit',
  // Do not inherit a caller's fork/FFI/config override into this offline check.
  env: Object.fromEntries(Object.entries(process.env).filter(([key]) =>
    !/^(FOUNDRY_|DAPP_|ETH_RPC_URL$|RPC_URL$)/.test(key))),
})
if (result.error) throw result.error
assert.equal(result.signal, null, `Forge terminated: ${result.signal}`)
process.exitCode = result.status ?? 1
