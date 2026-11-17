import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { releaseIdentity, verifiedImageResponse, verifyRevisionLabels, lookupRelease, verifyRelease, verifyTaskImages } from './runtime-release.mjs'

const sha = '1'.repeat(40), digest = 'sha256:' + '2'.repeat(64), child = 'sha256:' + '3'.repeat(64)
const identity = releaseIdentity('932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-api-sepolia', sha, 'https://github.com/Plether-Fi/plether-app')
const response = (d, manifest) => ({ failures: [], images: [{ imageId: { imageDigest: d }, imageManifest: JSON.stringify(manifest) }] })
const image = { mediaType: 'application/vnd.oci.image.manifest.v1+json', config: { digest }, layers: [] }
const labels = { 'org.opencontainers.image.revision': sha, 'org.opencontainers.image.source': identity.source, 'io.plether.build.run': identity.source + '/actions/runs/123', 'io.plether.build.attempt': '1' }

test('only explicit absent tags permit a new build', () => {
  assert.deepEqual(lookupRelease(identity, () => { throw { stderr: 'An error occurred (ImageNotFoundException)' } }), { exists: false, digest: '', image: '' })
  for (const stderr of ['Could not connect', 'AccessDeniedException', 'RepositoryNotFoundException']) {
    assert.throws(() => lookupRelease(identity, () => { throw { stderr } }))
  }
  assert.throws(() => lookupRelease(identity, () => ({ imageDetails: [] })))
})
test('a retained commit tag is reused by its existing digest and source labels must match', () => {
  assert.equal(lookupRelease(identity, () => ({ imageDetails: [{ imageDigest: digest }] })).image, identity.repository + '@' + digest)
  verifyRevisionLabels(labels, identity)
  for (const bad of [{}, { ...labels, 'org.opencontainers.image.revision': '4'.repeat(40) }, { ...labels, 'org.opencontainers.image.source': 'https://github.com/other/repo' }]) assert.throws(() => verifyRevisionLabels(bad, identity))
})
test('release indexes and all referenced manifests must be present before promotion', () => {
  const index = { manifests: [{ digest: child, platform: { os: 'linux', architecture: 'arm64' } }] }
  const calls = []
  const invoke = (_, args) => {
    const d = args.at(-1).slice('imageDigest='.length); calls.push(d)
    return response(d, d === digest ? index : image)
  }
  const docker = args => args[0] === 'pull' ? '' : JSON.stringify([{ Architecture: 'arm64', Os: 'linux', Config: { Labels: labels } }])
  assert.equal(verifyRelease(identity, digest, invoke, docker).digest, digest)
  assert.deepEqual(calls, [digest, child])
  assert.throws(() => verifyRelease(identity, digest, (_, args) => args.at(-1).endsWith(child) ? { failures: [{ failureCode: 'ImageNotFound' }], images: [] } : response(digest, index), docker))
  assert.throws(() => verifiedImageResponse(response(digest, { manifests: [] }), digest))
  assert.throws(() => verifiedImageResponse(response(child, image), digest))
})
test('ECR digest lookups accept identical entries returned once per tag and reject ambiguity', () => {
  const result = response(digest, image)
  result.images[0].imageId.imageTag = sha
  result.images.push({ ...structuredClone(result.images[0]), imageId: { imageDigest: digest, imageTag: 'latest' } })
  assert.deepEqual(verifiedImageResponse(result, digest), image)
  const conflict = structuredClone(result)
  conflict.images[1].imageManifest = JSON.stringify({ ...image, layers: [{ digest: child }] })
  assert.throws(() => verifiedImageResponse(conflict, digest), /conflicting/)
  const unexpected = structuredClone(result)
  unexpected.images[1].imageId.imageDigest = child
  assert.throws(() => verifiedImageResponse(unexpected, digest), /another digest/)
  assert.throws(() => verifiedImageResponse({ images: [], failures: [] }, digest))
  const missing = structuredClone(result)
  delete missing.images[1].imageManifest
  assert.throws(() => verifiedImageResponse(missing, digest), /invalid/)
  const task = { containerDefinitions: [{ image: identity.repository + '@' + digest }] }
  assert.equal(verifyTaskImages(task, identity.accountId, identity.region, () => result), 1)
})
test('selected task availability checks all main, init and router images before a service update', () => {
  const task = { containerDefinitions: [{ image: identity.repository + '@' + digest }, { image: identity.repository + '@' + digest }, { image: identity.repository + '-router:' + sha }] }
  const calls = []
  const invoke = (id, args) => {
    calls.push(id.repositoryName)
    return args[1] === 'describe-images' ? { imageDetails: [{ imageDigest: child }] } : response(args.at(-1).slice('imageDigest='.length), image)
  }
  assert.equal(verifyTaskImages(task, identity.accountId, identity.region, invoke), 2)
  assert.ok(calls.includes('plether-api-sepolia-router'))
  assert.throws(() => verifyTaskImages(task, '111111111111', identity.region, invoke))
  assert.throws(() => verifyTaskImages(task, identity.accountId, identity.region, () => ({ failures: [{ failureCode: 'ImageNotFound' }], images: [] })))
})
test('Alto uses the actual unambiguous digest-alias gate before selecting its manifest', () => {
  const workflow = readFileSync(new URL('../.github/workflows/deploy-alto.yml', import.meta.url), 'utf8')
  const filter = workflow.match(/if ! jq -e --arg digest "\$mirrored_digest" '([\s\S]*?)' \\\n\s+<<< "\$ecr_manifest_response"/)?.[1]
  assert.ok(filter)
  const fixture = () => {
    const r = response(digest, image)
    r.images[0].imageId.imageTag = sha
    r.images[0].imageManifestMediaType = 'application/vnd.oci.image.manifest.v1+json'
    r.images.push({ ...structuredClone(r.images[0]), imageId: { imageDigest: digest, imageTag: 'latest' } })
    return r
  }
  for (const [mutate, expected] of [
    [() => {}, true],
    [r => { r.images[1].imageId.imageDigest = child }, false],
    [r => { r.images[1].imageManifest += ' ' }, false],
    [r => { r.images[1].imageManifestMediaType = 'other' }, false],
    [r => { r.images = [] }, false],
  ]) {
    const r = fixture(); mutate(r)
    const result = spawnSync('jq', ['-e', '--arg', 'digest', digest, filter], { input: JSON.stringify(r), encoding: 'utf8' })
    assert.ifError(result.error)
    assert.equal(result.status === 0, expected, result.stderr)
  }
})
