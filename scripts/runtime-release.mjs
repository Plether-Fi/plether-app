import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { appendFileSync, readFileSync, writeFileSync } from 'node:fs'
import { pathToFileURL } from 'node:url'

export const digestPattern = /^sha256:[0-9a-f]{64}$/
export function releaseIdentity(repository, revision, source) {
  const match = /^(\d{12})\.dkr\.ecr\.([a-z0-9-]+)\.amazonaws\.com\/([a-z0-9][a-z0-9/_.-]*)$/.exec(repository)
  assert.ok(match, 'Release repository must be an explicit private ECR repository')
  assert.match(revision, /^[0-9a-f]{40}$/, 'Release revision must be a full Git SHA')
  assert.match(source, /^https:\/\/github\.com\/[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+$/, 'Release source must be a GitHub repository')
  return { accountId: match[1], region: match[2], repositoryName: match[3], repository, revision, source }
}

export function verifiedImageResponse(response, expectedDigest) {
  assert.match(expectedDigest, digestPattern)
  assert.deepEqual(response.failures, [], 'ECR manifest lookup was incomplete')
  // Digest lookups can return one entry per tag for the same manifest. They
  // are equivalent only when every digest and every unmodified manifest agree.
  assert.ok(Array.isArray(response.images) && response.images.length > 0, 'Expected an ECR manifest')
  assert.ok(response.images.every(image => image.imageId?.imageDigest === expectedDigest), 'ECR returned another digest')
  const raw = response.images[0].imageManifest
  assert.ok(typeof raw === 'string' && raw.length > 0 && response.images.every(image => image.imageManifest === raw), 'ECR returned conflicting or invalid manifests')
  const manifest = JSON.parse(raw)
  if (manifest.manifests) {
    const arm = manifest.manifests.filter(value => value.platform?.os === 'linux' && value.platform?.architecture === 'arm64')
    assert.equal(arm.length, 1, 'Release index must select exactly one Linux/ARM64 image')
    for (const child of manifest.manifests) assert.match(child.digest, digestPattern)
  } else {
    assert.ok(manifest.config && Array.isArray(manifest.layers), 'Expected an OCI/Docker image manifest')
  }
  return manifest
}

export function verifyRevisionLabels(labels, identity) {
  assert.equal(labels?.['org.opencontainers.image.revision'], identity.revision, 'Existing release tag has no matching source revision; never overwrite it')
  assert.equal(labels?.['org.opencontainers.image.source'], identity.source, 'Existing release tag has another source repository; never overwrite it')
  assert.ok(labels?.['io.plether.build.run']?.startsWith(`${identity.source}/actions/runs/`)
    && /^[1-9][0-9]*$/.test(labels['io.plether.build.run'].slice(`${identity.source}/actions/runs/`.length)), 'Release build run identity is missing or invalid')
  assert.match(labels?.['io.plether.build.attempt'] ?? '', /^[1-9][0-9]*$/, 'Release build attempt is missing')
}

function aws(identity, args) {
  return JSON.parse(execFileSync('aws', ['--region', identity.region, ...args, '--output', 'json', '--no-cli-pager'], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }))
}
export function lookupRelease(identity, invoke = aws) {
  try {
    const result = invoke(identity, ['ecr', 'describe-images', '--registry-id', identity.accountId, '--repository-name', identity.repositoryName, '--image-ids', `imageTag=${identity.revision}`])
    assert.equal(result.imageDetails?.length, 1, 'Release tag lookup was incomplete')
    const digest = result.imageDetails[0].imageDigest
    assert.match(digest, digestPattern)
    return { exists: true, digest, image: `${identity.repository}@${digest}` }
  } catch (error) {
    // Only an explicit missing tag permits a build. Connectivity/authentication
    // errors must not be mistaken for permission to replace an existing release.
    if (String(error.stderr ?? '').includes('(ImageNotFoundException)')) return { exists: false, digest: '', image: '' }
    throw error
  }
}

function verifyManifestGraph(identity, digest, invoke, visited = new Set(), ancestors = new Set()) {
  assert.ok(!ancestors.has(digest) && ancestors.size < 8 && visited.size < 1000, 'Invalid or excessive OCI manifest graph')
  if (visited.has(digest)) return
  const manifest = verifiedImageResponse(invoke(identity, ['ecr', 'batch-get-image', '--registry-id', identity.accountId, '--repository-name', identity.repositoryName, '--image-ids', `imageDigest=${digest}`]), digest)
  visited.add(digest)
  for (const child of manifest.manifests ?? []) verifyManifestGraph(identity, child.digest, invoke, visited, new Set([...ancestors, digest]))
}
export function verifyRelease(identity, digest, invoke = aws, docker = (args) => execFileSync('docker', args, { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] })) {
  verifyManifestGraph(identity, digest, invoke)
  const image = `${identity.repository}@${digest}`
  docker(['pull', '--platform', 'linux/arm64', image])
  const inspection = JSON.parse(docker(['image', 'inspect', image]))
  assert.equal(inspection.length, 1)
  assert.equal(inspection[0].Architecture, 'arm64')
  assert.equal(inspection[0].Os, 'linux')
  verifyRevisionLabels(inspection[0].Config?.Labels, identity)
  return { ...identity, digest, image, platform: 'linux/arm64', buildRun: inspection[0].Config.Labels['io.plether.build.run'], buildAttempt: inspection[0].Config.Labels['io.plether.build.attempt'] }
}

export function verifyTaskImages(task, accountId, region, invoke = aws) {
  assert.ok(Array.isArray(task.containerDefinitions) && task.containerDefinitions.length > 0)
  const images = [...new Set(task.containerDefinitions.map(c => c.image))]
  for (const image of images) {
    const match = /^(\d{12}\.dkr\.ecr\.[a-z0-9-]+\.amazonaws\.com\/[a-z0-9][a-z0-9/_.-]*)(?:@((?:sha256:)[0-9a-f]{64})|:([A-Za-z0-9_.-]+))$/.exec(image)
    assert.ok(match, 'Runtime task requires explicit private ECR image identities')
    const identity = releaseIdentity(match[1], '0'.repeat(40), 'https://github.com/Plether-Fi/plether-app')
    assert.equal(identity.accountId, accountId, 'Runtime image account mismatch')
    assert.equal(identity.region, region, 'Runtime image region mismatch')
    let digest = match[2]
    if (!digest) {
      const response = invoke(identity, ['ecr', 'describe-images', '--registry-id', identity.accountId, '--repository-name', identity.repositoryName, '--image-ids', `imageTag=${match[3]}`])
      assert.equal(response.imageDetails?.length, 1, 'Runtime image tag is missing')
      digest = response.imageDetails[0].imageDigest
    }
    verifyManifestGraph(identity, digest, invoke)
  }
  return images.length
}

function options(args) {
  const result = {}
  for (let i = 0; i < args.length; i += 2) {
    assert.match(args[i], /^--[a-z-]+$/)
    assert.ok(args[i + 1] && !args[i + 1].startsWith('--'))
    assert.equal(result[args[i].slice(2)], undefined, 'Duplicate argument')
    result[args[i].slice(2)] = args[i + 1]
  }
  return result
}
if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  try {
    const [command, ...args] = process.argv.slice(2), o = options(args)
    if (command === 'verify-task') {
      const task = JSON.parse(readFileSync(o['task-definition'], 'utf8'))
      const count = verifyTaskImages(task, o.account, o.region)
      console.log(`Verified ${count} currently selected runtime image dependencies`)
      process.exit(0)
    }
    const identity = releaseIdentity(o.repository, o.revision, o.source)
    assert.match(o.prefix ?? '', /^(application|router)$/)
    if (command === 'lookup') {
      const found = lookupRelease(identity)
      if (found.exists) verifyRelease(identity, found.digest)
      appendFileSync(o.output, `${o.prefix}_exists=${found.exists}\n${o.prefix}_digest=${found.digest}\n`)
    } else if (command === 'verify') {
      const retained = lookupRelease(identity)
      assert.ok(retained.exists && retained.digest === o.digest, 'The immutable release tag does not select the built/reused digest')
      const release = verifyRelease(identity, o.digest)
      appendFileSync(o.output, `${o.prefix === 'application' ? 'image' : 'log_router_image'}=${release.image}\n`)
      writeFileSync(o.record, JSON.stringify({ schema: 1, ...release, workflowRun: process.env.GITHUB_RUN_ID, workflowAttempt: process.env.GITHUB_RUN_ATTEMPT }, null, 2) + '\n')
      console.log(`Verified ${release.platform} release ${release.revision} at ${release.digest}`)
    } else throw Error('Expected lookup or verify')
  } catch (error) {
    // Provider errors can contain endpoints; diagnostics expose a fixed class.
    console.error(error instanceof assert.AssertionError ? error.message : 'Release verification failed; no image was promoted')
    process.exitCode = 1
  }
}
