import fs from 'node:fs'
import path from 'node:path'
import { execFileSync } from 'node:child_process'

const core = process.argv[2]
if (!core) throw new Error('Usage: node scripts/generate-protection-worker-abi.mjs <built-core-v1.2.1>')
const release = JSON.parse(fs.readFileSync('config/perps/arbitrum-sepolia-v2.json'))
// Imported structs/interfaces affect the ABI just as much as the facade files.
execFileSync('git', ['-C', core, 'diff', '--exit-code', '--quiet', release.release.sourceCommit, '--', 'packages/perps/src', 'foundry.toml'])
execFileSync('git', ['-C', core, 'diff', '--exit-code', '--quiet', release.release.sourceCommit, 'HEAD', '--', 'lib', 'foundry.toml'])
for (const name of ['PositionProtectionBook', 'OrderRouter', 'OrderLifecycleBook', 'PletherOracle']) {
  const source = `packages/perps/src/${name}.sol`
  const current = execFileSync('git', ['-C', core, 'hash-object', source], { encoding: 'utf8' }).trim()
  const pinned = execFileSync('git', ['-C', core, 'rev-parse', `${release.release.sourceCommit}:${source}`], { encoding: 'utf8' }).trim()
  if (current !== pinned) throw new Error(`Source mismatch for ${name}`)
}
const contracts = Object.fromEntries(['PositionProtectionBook', 'OrderRouter', 'OrderLifecycleBook', 'PletherOracle'].map(name => [name, JSON.parse(fs.readFileSync(path.join(core, `out/${name}.sol/${name}.json`))).abi]))
fs.writeFileSync('apps/backend/protection-worker/abi.mjs', `// Generated from perps v1.2.1 ${release.release.sourceCommit}. Do not edit.\nexport default ${JSON.stringify(contracts)}\n`)
