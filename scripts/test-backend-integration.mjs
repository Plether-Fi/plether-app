import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

// Historical deployed scenarios supply validated fixtures at their runtime
// boundaries. Compile the actual manifest so prepared-release guards are tested
// alongside those scenarios rather than replaced during the build.
const backend = fileURLToPath(new URL('../apps/backend/', import.meta.url))
const result = spawnSync('cabal', ['test', 'plether-api-integration-test', '--test-show-details=direct', '-j1', ...process.argv.slice(2)],
  { cwd: backend, env: process.env, stdio: 'inherit' })
if (result.error) throw result.error
process.exitCode = result.status ?? 1
