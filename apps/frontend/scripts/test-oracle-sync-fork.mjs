import { mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { spawnSync } from 'node:child_process'

if (!process.env.ARB_SEPOLIA_RPC_URL) throw new Error('Set ARB_SEPOLIA_RPC_URL (fork only; never broadcasts)')
const root = mkdtempSync(resolve(tmpdir(), 'oracle-sync-fork-'))
const test = fileURLToPath(new URL('../test/oracle-sync-fork', import.meta.url))
writeFileSync(resolve(root, 'foundry.toml'), `[profile.default]\nsrc = "src"\ntest = ${JSON.stringify(test)}\nsolc = "0.8.35"\noptimizer = false\nvia_ir = true\n`)
const result = spawnSync('forge', ['test', '--root', root, '-vvv'], { stdio: 'inherit' })
if (result.error) throw result.error
process.exitCode = result.status ?? 1
