import { cpSync, copyFileSync, mkdirSync, mkdtempSync, rmSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { tmpdir } from 'node:os'
import { basename, join } from 'node:path'
import { fileURLToPath } from 'node:url'

// These tests exercise a deployed market. Build with synthetic pins in an
// isolated source tree; never alter the prepared release or publish binaries.
const root = fileURLToPath(new URL('..', import.meta.url))
const directory = mkdtempSync(join(tmpdir(), 'plether-backend-integration-'))
try {
  const backend = join(directory, 'apps/backend')
  cpSync(join(root, 'apps/backend'), backend, { recursive: true,
    filter: path => !['dist-newstyle', 'node_modules', '.env'].includes(basename(path)) })
  cpSync(join(root, 'config'), join(directory, 'config'), { recursive: true })
  mkdirSync(join(directory, 'scripts'), { recursive: true })
  cpSync(join(root, 'scripts/fixtures'), join(directory, 'scripts/fixtures'), { recursive: true })
  copyFileSync(join(root, 'apps/frontend/src/contracts/__tests__/fixtures/perps-canonical-release.json'),
    join(directory, 'config/perps/mock-usdg-prepared.json'))
  const result = spawnSync('cabal', ['test', 'plether-api-integration-test', '--test-show-details=direct', '-j1', ...process.argv.slice(2)],
    { cwd: backend, env: process.env, stdio: 'inherit' })
  if (result.error) throw result.error
  process.exitCode = result.status ?? 1
} finally {
  rmSync(directory, { recursive: true, force: true })
}
