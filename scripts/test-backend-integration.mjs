import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

// Historical deployed scenarios supply validated fixtures at their runtime
// boundaries. Compile the selected v2.1.0 manifest without replacing its deployment pins.
// Prepared-profile guards are exercised explicitly alongside deployed scenarios.
const backend = fileURLToPath(new URL('../apps/backend/', import.meta.url))
// Compile with full parallelism before running the test suite serially (-j1).
const build = spawnSync('cabal', ['build', 'plether-api-integration-test'],
  { cwd: backend, env: process.env, stdio: 'inherit' })
if (build.error) throw build.error
if ((build.status ?? 1) !== 0) {
  process.exitCode = build.status ?? 1
} else {
  const result = spawnSync('cabal', ['test', 'plether-api-integration-test', '--test-show-details=direct', '-j1', ...process.argv.slice(2)],
    { cwd: backend, env: process.env, stdio: 'inherit' })
  if (result.error) throw result.error
  process.exitCode = result.status ?? 1
}
