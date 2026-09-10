import fs from 'node:fs'
import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'

const bundle = process.argv[2]
if (!bundle) throw new Error('Usage: node scripts/generate-protection-worker-abi.mjs <release-bundle.tar.gz>')
const { release } = JSON.parse(fs.readFileSync('config/perps/arbitrum-sepolia-v2.json'))
const digest = createHash('sha256').update(fs.readFileSync(bundle)).digest('hex')
if (digest !== release.bundleSha256) throw new Error(`Expected the pinned ${release.version} ABI bundle`)
const artifacts = { PositionProtectionBook: 'PositionProtectionBook', OrderRouter: 'ArbitrumSepoliaReleaseRouter', OrderLifecycleBook: 'OrderLifecycleBook', PletherOracle: 'ArbitrumSepoliaReleaseOracle' }
const contracts = Object.fromEntries(Object.entries(artifacts).map(([name, artifact]) => [name,
  JSON.parse(execFileSync('tar', ['-xOzf', bundle, `perps-${release.version}-arbitrum-sepolia/abi/${artifact}.json`], { encoding: 'utf8' })),
]))
fs.writeFileSync('apps/backend/protection-worker/abi.mjs', `// Generated from perps ${release.version} ${release.sourceCommit}, checksum-verified release bundle. Do not edit.\nexport default ${JSON.stringify(contracts)}\n`)
