import test from 'node:test'
import assert from 'node:assert/strict'
import { migrateDiagramEvidence, packageName, removeLegacyLockEntries, replaceSection, validatePublishedRelease } from './migrate-perps-aa-client.mjs'

const sha = 'a'.repeat(40)
const metadata = { name: packageName, version: '0.1.0', gitHead: sha,
  dist: { integrity: `sha512-${Buffer.alloc(64).toString('base64')}`, tarball: 'https://npm.pkg.github.com/download/@plether-fi/perps-aa-client/0.1.0/fixture' } }

test('AA evidence migration preserves the independent protocol review and diagram content', () => {
  const source = `https://github.com/Plether-Fi/plether-core/blob/${sha}/packages/perps-aa-client`
  const original = {
    reviewedRelease: { version: 'v1.2.2', sourceCommit: 'b'.repeat(40) },
    diagrams: [{ title: 'Preserved', evidence: ['protocol/source.sol',
      'apps/frontend/vendor/perps-aa-client/dist/actions.js',
      'apps/frontend/vendor/perps-aa-client/dist/orchestrator.js:sendSponsoredAction'] }],
  }
  const updated = JSON.parse(migrateDiagramEvidence(JSON.stringify(original), source))
  assert.deepEqual(updated.reviewedRelease, original.reviewedRelease)
  assert.deepEqual(updated.diagrams, [{ title: 'Preserved', evidence: [
    'protocol/source.sol', `${source}/src/actions.ts`, `${source}/src/orchestrator.ts`,
  ] }])
  assert.equal(migrateDiagramEvidence(JSON.stringify(updated), source), JSON.stringify(updated))
})

test('migration accepts only an immutable public release with matching source and tag', () => {
  const record = validatePublishedRelease(metadata, sha, '0.1.0', sha, 'public')
  assert.equal(record.sourceCommit, sha)
  assert.equal(record.tag, 'perps-aa-client-v0.1.0')
  assert.throws(() => validatePublishedRelease(metadata, sha, '0.1.0', 'b'.repeat(40), 'public'))
  assert.throws(() => validatePublishedRelease(metadata, sha, '0.1.0', sha, 'private'))
  assert.throws(() => validatePublishedRelease({ ...metadata, gitHead: 'b'.repeat(40) }, sha, '0.1.0', sha, 'public'))
})

test('migration rejects untrusted tarball locations, credentials, and missing integrity', () => {
  for (const dist of [
    { ...metadata.dist, tarball: 'https://example.com/package.tgz' },
    { ...metadata.dist, tarball: 'https://user:secret@npm.pkg.github.com/package.tgz' },
    { ...metadata.dist, tarball: 'https://npm.pkg.github.com/package.tgz?token=secret' },
    { ...metadata.dist, integrity: undefined },
  ]) assert.throws(() => validatePublishedRelease({ ...metadata, dist }, sha, '0.1.0', sha, 'public'))
})

test('documentation migration preserves surrounding text and refuses ambiguous sections', () => {
  assert.equal(replaceSection('intro\nSTART\nold\nEND\nfooter', 'START', 'END', 'new'), 'intro\nnew\n\nEND\nfooter')
  assert.throws(() => replaceSection('START\nSTART\nEND', 'START', 'END', 'new'))
  assert.throws(() => replaceSection('changed\nEND', 'START', 'END', 'new'))
})

test('migration removes npm orphan vendor entries without disturbing the registry dependency', () => {
  const registryPackage = { version: '0.1.0', resolved: metadata.dist.tarball, integrity: metadata.dist.integrity }
  const lock = { lockfileVersion: 3, packages: {
    '': { dependencies: { [packageName]: '0.1.0' } },
    [`node_modules/${packageName}`]: registryPackage,
    'vendor/perps-aa-client': { name: '@plether/perps-aa-client', version: '0.1.0' },
  } }
  const cleaned = removeLegacyLockEntries(lock)
  assert.strictEqual(cleaned.packages[`node_modules/${packageName}`], registryPackage)
  assert.equal(cleaned.packages['vendor/perps-aa-client'], undefined)
  assert.throws(() => removeLegacyLockEntries({ lockfileVersion: 3, packages: {
    '': { dependencies: { '@plether/perps-aa-client': 'file:vendor/perps-aa-client' } },
  } }))
})
