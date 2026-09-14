import { mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { spawnSync } from 'node:child_process'

// Source supplies ABI interfaces/test utilities only. The test checks hashes
// and executes the actual deployed contracts on a local fork, never broadcasts.
const core = process.env.PLETHER_CORE_PATH
if (!core || !process.env.ARB_SEPOLIA_RPC_URL) throw new Error('Set PLETHER_CORE_PATH and ARB_SEPOLIA_RPC_URL')
const root = mkdtempSync(resolve(tmpdir(), 'close-preview-fork-'))
const test = fileURLToPath(new URL('../test/close-preview-fork', import.meta.url))
const remappings = [
  ['forge-std/', 'lib/forge-std/src'],
  ['@openzeppelin/contracts/', 'lib/openzeppelin-contracts/contracts'],
  ['@plether/perps/', 'packages/perps/src'],
  ['@plether/shared/', 'packages/shared/src'],
].map(([prefix, relative]) => `${prefix}=${resolve(core, relative)}/`)
// No optimization is needed for the test harness; it also avoids solc's
// optimizer stack limit for comparisons of the large deployed return tuples.
writeFileSync(resolve(root, 'foundry.toml'), `[profile.default]\nsrc = "src"\ntest = ${JSON.stringify(test)}\nsolc = "0.8.35"\noptimizer = false\nvia_ir = true\nauto_detect_remappings = false\nremappings = ${JSON.stringify(remappings)}\n`)
const result = spawnSync('forge', ['test', '--root', root, '-vvv'], { stdio: 'inherit' })
if (result.error) throw result.error
process.exitCode = result.status ?? 1
