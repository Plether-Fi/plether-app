import test from 'node:test'
import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import { accessSync, constants, copyFileSync, mkdirSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { delimiter, dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const root = fileURLToPath(new URL('../', import.meta.url))
const guard = join(root, 'scripts/check-no-blockscout-runtime.sh')
const fixturePath = 'apps/frontend/src/contracts/__tests__/fixtures/perps-canonical-aa.json'
const manifestPath = 'apps/frontend/public/perps-aa-manifest.json'
const binary = name => {
  for (const directory of process.env.PATH.split(delimiter)) {
    const path = join(directory, name)
    try { accessSync(path, constants.X_OK); return path } catch { /* next PATH entry */ }
  }
  throw new Error(`Required test tool unavailable: ${name}`)
}
const bash = binary('bash')
const git = binary('git')

function tools(t) {
  const directory = mkdtempSync(join(tmpdir(), 'blockscout-tools-'))
  t.after(() => rmSync(directory, { recursive: true, force: true }))
  for (const name of ['bash', 'git', 'dirname', 'awk']) symlinkSync(binary(name), join(directory, name))
  return directory
}

function scan(script, path) {
  const env = { ...process.env, PATH: path }
  delete env.GIT_DIR
  delete env.GIT_WORK_TREE
  const result = spawnSync(bash, [script], { env, encoding: 'utf8' })
  assert.ifError(result.error)
  return result
}

function repository(t, files = {}, tracked = true) {
  const directory = mkdtempSync(join(tmpdir(), 'blockscout-scan-'))
  t.after(() => rmSync(directory, { recursive: true, force: true }))
  mkdirSync(join(directory, 'scripts'))
  copyFileSync(guard, join(directory, 'scripts/check-no-blockscout-runtime.sh'))
  for (const [name, value] of Object.entries(files)) {
    mkdirSync(dirname(join(directory, name)), { recursive: true })
    writeFileSync(join(directory, name), value)
  }
  if (tracked) {
    for (const args of [['init', '-q'], ['add', '--', '.']]) {
      const result = spawnSync(git, args, { cwd: directory, encoding: 'utf8' })
      assert.equal(result.status, 0, result.stderr)
    }
  }
  return join(directory, 'scripts/check-no-blockscout-runtime.sh')
}

test('the actual tracked repository permits reviewed explorer links without ripgrep', t => {
  const path = tools(t)
  assert.equal(spawnSync('rg', ['--version'], { env: { PATH: path } }).error?.code, 'ENOENT')
  const result = scan(guard, path)
  assert.equal(result.status, 0, result.stderr)
  assert.match(result.stdout, /No Blockscout runtime HTTP\/RPC targets found/)
})

test('a successful tracked-source scan with no matches passes', t => {
  const script = repository(t, { 'apps/backend/src/Safe.hs': 'rpc = "https://rpc.example"\n' })
  assert.equal(scan(script, tools(t)).status, 0)
})

for (const path of ['apps/backend/src/Unsafe.hs', 'apps/frontend/src/unsafeRpc.ts', 'infra/terraform/terraform.tfvars.example']) {
  test(`a new tracked runtime target in ${path} fails without ripgrep`, t => {
    const script = repository(t, { [path]: 'rpc_url = "https://arbitrum-sepolia.blockscout.com/api/eth-rpc"\n' })
    const result = scan(script, tools(t))
    assert.equal(result.status, 1, result.stderr)
    assert.ok(result.stderr.includes(path))
  })
}

for (const path of [fixturePath, manifestPath]) {
  test(`only explorer fields are allowed in ${path}`, t => {
    const manifest = JSON.parse(readFileSync(join(root, path), 'utf8'))
    const script = repository(t, { [path]: JSON.stringify(manifest, null, 2) + '\n' })
    const toolPath = tools(t)
    assert.equal(scan(script, toolPath).status, 0)
    manifest.bundlerRpcUrl = 'https://arbitrum-sepolia.blockscout.com/api/eth-rpc'
    writeFileSync(join(dirname(dirname(script)), path), JSON.stringify(manifest, null, 2) + '\n')
    const result = scan(script, toolPath)
    assert.equal(result.status, 1, result.stderr)
    assert.match(result.stderr, /bundlerRpcUrl/)
  })
}

test('the reviewed explorer utility cannot conceal a new runtime target', t => {
  const path = 'apps/frontend/src/utils/explorer.ts'
  const content = readFileSync(join(root, path), 'utf8')
  const script = repository(t, { [path]: content + '\nexport const rpc = "https://arbitrum-sepolia.blockscout.com/api/eth-rpc"\n' })
  const result = scan(script, tools(t))
  assert.equal(result.status, 1, result.stderr)
  assert.match(result.stderr, /export const rpc/)
})

test('scanner tool failures propagate instead of reporting a clean repository', t => {
  const path = tools(t)
  rmSync(join(path, 'git'))
  writeFileSync(join(path, 'git'), '#!/usr/bin/env bash\necho "synthetic git scan failure" >&2\nexit 2\n', { mode: 0o755 })
  const result = scan(guard, path)
  assert.equal(result.status, 2)
  assert.match(result.stderr, /source scan failed \(git grep exit 2\)/)
  assert.doesNotMatch(result.stdout, /No Blockscout/)
})

test('a real git error outside a repository fails the scan', t => {
  const script = repository(t, {}, false)
  const result = scan(script, tools(t))
  assert.equal(result.status, 128)
  assert.match(result.stderr, /source scan failed \(git grep exit 128\)/)
})
