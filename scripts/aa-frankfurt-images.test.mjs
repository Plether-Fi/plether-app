import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { createHash } from 'node:crypto'
import { imageTarget, validateImageRecord, manifestIdentity } from './aa-frankfurt-images.mjs'

const fixture = () => ({ ...imageTarget, coreRelease: 'v1.2.3', images: ['api', 'otel-log-router', 'alto'].map(name => {
  const repository = `plether-${name}-sepolia-aa-temp`
  const digest = name === 'alto' ? imageTarget.altoSource.split('@')[1] : `sha256:${'a'.repeat(64)}`
  return { name, repository, digest, reference: `${imageTarget.registry}/${repository}@${digest}`,
    tag: name === 'alto' ? imageTarget.altoTag : `source-${imageTarget.sourceCommit}`, platform: 'linux/arm64',
    ...(name === 'alto' ? { upstreamReference: imageTarget.altoSource, contentVerified: true } : {}) }
}) })

test('image evidence requires exact Frankfurt repositories, immutable digests and source', () => {
  validateImageRecord(fixture())
  for (const mutate of [
    r => { r.region = 'ap-southeast-1' },
    r => { r.account = '111111111111' },
    r => { r.sourceCommit = 'b'.repeat(40) },
    r => { r.images[0].repository = 'plether-api-sepolia' },
    r => { r.images[0].reference = 'registry/image:latest' },
    r => { r.images[0].platform = 'linux/amd64' },
    r => { r.images[0].tag = 'latest' },
    r => { r.images[2].digest = `sha256:${'b'.repeat(64)}` },
    r => { r.images[2].contentVerified = false },
    r => { r.images.pop() },
  ]) { const record = fixture(); mutate(record); assert.throws(() => validateImageRecord(record)) }
})

test('Alto mirror permits media-type conversion but never changed or reordered content', () => {
  const source = { schemaVersion: 2, config: { digest: `sha256:${'a'.repeat(64)}`, size: 123 },
    layers: [{ digest: `sha256:${'b'.repeat(64)}`, size: 456 }, { digest: `sha256:${'c'.repeat(64)}`, size: 789 }] }
  assert.deepEqual(manifestIdentity({ ...source, mediaType: 'converted' }), manifestIdentity(source))
  assert.notDeepEqual(manifestIdentity({ ...source, layers: [...source.layers].reverse() }), manifestIdentity(source))
  assert.throws(() => manifestIdentity({ ...source, config: { digest: 'wrong', size: 123 } }))
})

test('image preparer never activates services, reads runtime secrets or builds dirty files', () => {
  const source = readFileSync(new URL('./aa-frankfurt-images.mjs', import.meta.url), 'utf8')
  assert.match(source, /\['archive', imageTarget.sourceCommit/)
  assert.match(source, /--network', 'none'/)
  assert.match(source, /--password-stdin/)
  assert.match(source, /if \(!existing\) docker\(\['push'/)
  for (const forbidden of ['update-service', 'register-task-definition', 'run-task', 'get-parameter', 'loadPreparedInputs', 'terraform apply']) {
    assert(!source.includes(forbidden), forbidden)
  }
})

test('Frankfurt build recipe is hash-pinned and preserves package verification', () => {
  const recipe = readFileSync(new URL('./frankfurt-backend.Dockerfile', import.meta.url), 'utf8')
  assert.equal(createHash('sha256').update(recipe).digest('hex'), imageTarget.backendRecipeSha256)
  assert.equal((recipe.match(/COPY --from=ca-trust/g) ?? []).length, 2)
  assert.equal((recipe.match(/https:\/\/deb\.debian\.org/g) ?? []).length, 2)
  for (const bypass of ['--allow-unauthenticated', 'Verify-Peer=false', 'trusted=yes', '--insecure']) assert(!recipe.includes(bypass))
})
