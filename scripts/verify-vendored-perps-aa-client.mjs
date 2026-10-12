import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { execFileSync } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'

const directory = new URL('../apps/frontend/vendor/perps-aa-client/', import.meta.url)
const release = JSON.parse(readFileSync(new URL('release.json', directory)))
assert.equal(release.name, '@plether-fi/perps-aa-client')
assert.equal(release.version, '0.2.0')
assert.match(release.gitHead, /^[0-9a-f]{40}$/)
assert.equal(release.filename, `plether-fi-perps-aa-client-${release.version}.tgz`)
const tarball = new URL(release.filename, directory)
assert.equal(`sha512-${createHash('sha512').update(readFileSync(tarball)).digest('base64')}`, release.integrity)
const metadata = JSON.parse(execFileSync('tar', ['-xOf', fileURLToPath(tarball), 'package/package.json'], { encoding: 'utf8' }))
assert.equal(metadata.gitHead, release.gitHead)
assert.equal(metadata.version, release.version)
assert.equal(metadata.name, release.name)
const spec = `file:vendor/perps-aa-client/${release.filename}`
const pkg = JSON.parse(readFileSync(new URL('../apps/frontend/package.json', import.meta.url)))
const lock = JSON.parse(readFileSync(new URL('../apps/frontend/package-lock.json', import.meta.url)))
assert.equal(pkg.dependencies[release.name], spec)
assert.equal(lock.packages[''].dependencies[release.name], spec)
assert.equal(lock.packages[`node_modules/${release.name}`].resolved, spec)
assert.equal(lock.packages[`node_modules/${release.name}`].integrity, release.integrity)
console.log(`Verified local SDK ${release.version} from Core ${release.gitHead}; no registry publication`)
