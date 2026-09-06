import fs from 'node:fs'
import path from 'node:path'
import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'

// Rebuild/test the package at an immutable upstream commit before invoking.
const upstream = process.argv[2]
if (!upstream) throw new Error('Usage: node scripts/vendor-perps-aa-client.mjs <clean-core-worktree>')
const commit = execFileSync('git', ['-C', upstream, 'rev-parse', 'HEAD'], { encoding: 'utf8' }).trim()
const dirty = execFileSync('git', ['-C', upstream, 'status', '--porcelain', '--', 'packages/perps-aa-client'], { encoding: 'utf8' }).trim()
if (dirty) throw new Error('Commit upstream package source before vendoring')
const source = path.join(upstream, 'packages/perps-aa-client')
const destination = 'apps/frontend/vendor/perps-aa-client'
const pkg = JSON.parse(fs.readFileSync(path.join(source, 'package.json'), 'utf8'))
fs.cpSync(path.join(source, 'dist'), path.join(destination, 'dist'), { recursive: true })
for (const [from, to] of [['README.md', 'README.md'], ['package.json', 'upstream-package.json']]) {
  fs.copyFileSync(path.join(source, from), path.join(destination, to))
}
const local = JSON.parse(fs.readFileSync(path.join(destination, 'package.json'), 'utf8'))
local.pletherVendoredFrom = { repository: 'plether-core', path: 'packages/perps-aa-client', commit }
for (const key of ['version', 'main', 'types', 'exports', 'dependencies']) local[key] = pkg[key]
fs.writeFileSync(path.join(destination, 'package.json'), JSON.stringify(local, null, 2) + '\n')
const provenance = fs.readFileSync(path.join(destination, 'UPSTREAM.md'), 'utf8').replace(/Reviewed commit: `[^`]+`/, `Reviewed commit: \`${commit}\``)
fs.writeFileSync(path.join(destination, 'UPSTREAM.md'), provenance)
const files = ['README.md', 'upstream-package.json', ...fs.readdirSync(path.join(destination, 'dist')).map(file => `dist/${file}`)].sort()
fs.writeFileSync(path.join(destination, 'SHA256SUMS'), files.map(file => `${createHash('sha256').update(fs.readFileSync(path.join(destination, file))).digest('hex')}  ${file}\n`).join(''))
console.log(`Vendored perps-aa-client from ${commit}`)
