#!/usr/bin/env node
// Image preparation only: no ECS registration/update, task execution, Terraform,
// secret reads, KMS signing, funding, or on-chain transactions.
import { execFileSync } from 'node:child_process'
import { mkdtempSync, mkdirSync, readFileSync, writeFileSync, rmSync } from 'node:fs'
import { tmpdir, homedir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { awsJson, verifyAccount } from './frankfurt-aa-secrets.mjs'

export const imageTarget = {
  account: '932542905614', region: 'eu-central-1', deployment: 'sepolia-aa-temp',
  sourceCommit: 'ab272612c29b4febd502bc2735e7e0d4d560fb21',
  sourceTree: '8c13f2d4f062e75bd6722d606f0e577f8aecd42e',
  backendRecipeSha256: '5534710ccd61653e6d21b48f56905ad1f2c97abea36beeaba834e9527d69ab9b',
  registry: '932542905614.dkr.ecr.eu-central-1.amazonaws.com',
  altoSource: 'ghcr.io/pimlicolabs/alto:v1.2.7@sha256:28cee87ea6b58ba10a37273e58602b50321516c36a81d0c35d50526d1f06995d',
  altoTag: 'v1.2.7-arm64-28cee87ea6b5',
}

export function validateImageRecord(record) {
  assert.equal(record.account, imageTarget.account)
  assert.equal(record.region, imageTarget.region)
  assert.equal(record.deployment, imageTarget.deployment)
  assert.equal(record.sourceCommit, imageTarget.sourceCommit)
  assert.equal(record.sourceTree, imageTarget.sourceTree)
  assert.equal(record.backendRecipeSha256, imageTarget.backendRecipeSha256)
  assert.equal(record.coreRelease, 'v1.2.3')
  assert.equal(record.images?.length, 3)
  for (const name of ['api', 'otel-log-router', 'alto']) {
    const image = record.images.find(item => item.name === name)
    assert(image, `Missing ${name}`)
    assert.equal(image.repository, `plether-${name}-sepolia-aa-temp`)
    assert.equal(image.platform, 'linux/arm64')
    assert.match(image.digest, /^sha256:[a-f0-9]{64}$/)
    assert.equal(image.reference, `${imageTarget.registry}/${image.repository}@${image.digest}`)
    assert.equal(image.tag, name === 'alto' ? imageTarget.altoTag : `source-${imageTarget.sourceCommit}`)
    if (name === 'alto') {
      assert.equal(image.upstreamReference, imageTarget.altoSource)
      assert.equal(image.contentVerified, true)
    }
  }
  return record
}

// Same content-identity rule as deploy-alto.yml: registries may rewrite
// descriptor media types while preserving the config and ordered layer bytes.
export function manifestIdentity(manifest) {
  assert.equal(manifest.schemaVersion, 2)
  const descriptor = value => {
    assert.match(value.digest, /^sha256:[a-f0-9]{64}$/)
    assert(Number.isInteger(value.size) && value.size > 0)
    return { digest: value.digest, size: value.size }
  }
  assert(manifest.layers?.length > 0)
  return { config: descriptor(manifest.config), orderedLayers: manifest.layers.map(descriptor) }
}

async function approvedAltoManifest() {
  const response = await fetch('https://ghcr.io/token?scope=repository:pimlicolabs/alto:pull', { signal: AbortSignal.timeout(30000) })
  assert(response.ok, `GHCR public pull token HTTP ${response.status}`)
  const { token } = await response.json()
  const read = async digest => {
    assert.match(digest, /^sha256:[a-f0-9]{64}$/)
    const result = await fetch(`https://ghcr.io/v2/pimlicolabs/alto/manifests/${digest}`, {
      signal: AbortSignal.timeout(30000), headers: { Authorization: `Bearer ${token}`,
        Accept: 'application/vnd.oci.image.manifest.v1+json, application/vnd.docker.distribution.manifest.v2+json, application/vnd.oci.image.index.v1+json, application/vnd.docker.distribution.manifest.list.v2+json' },
    })
    assert(result.ok, `GHCR manifest HTTP ${result.status}`)
    const bytes = Buffer.from(await result.arrayBuffer())
    assert.equal(`sha256:${createHash('sha256').update(bytes).digest('hex')}`, digest, 'Upstream manifest bytes do not match the approved digest')
    return JSON.parse(bytes)
  }
  let manifest = await read(imageTarget.altoSource.split('@')[1])
  if (manifest.manifests) {
    const candidates = manifest.manifests.filter(item => item.platform?.os === 'linux' && item.platform.architecture === 'arm64')
    assert.equal(candidates.length, 1)
    manifest = await read(candidates[0].digest)
  }
  return manifest
}

async function main() {
  assert.deepEqual(process.argv.slice(2), ['--publish'], 'Usage: node scripts/aa-frankfurt-images.mjs --publish')
  const root = fileURLToPath(new URL('../', import.meta.url))
  const git = args => execFileSync('git', args, { cwd: root, encoding: 'utf8' }).trim()
  assert.equal(git(['rev-parse', `${imageTarget.sourceCommit}^{tree}`]), imageTarget.sourceTree)
  // Only the immutable committed source is sent to Docker, never this dirty
  // worktree, private plans, local .env files, or the user's Docker credentials.
  const release = JSON.parse(git(['show', `${imageTarget.sourceCommit}:config/perps/arbitrum-sepolia-v2.json`]))
  assert.equal(release.release.version, 'v1.2.3')
  const backendRecipe = join(root, 'scripts/frankfurt-backend.Dockerfile')
  assert.equal(createHash('sha256').update(readFileSync(backendRecipe)).digest('hex'), imageTarget.backendRecipeSha256)
  const socket = `unix://${join(homedir(), '.docker/run/docker.sock')}`
  const dockerArgs = ['--host', socket]
  execFileSync('docker', [...dockerArgs, 'version'], { timeout: 15000, stdio: 'ignore' })
  verifyAccount()
  const repos = ['api', 'otel-log-router', 'alto'].map(name => `plether-${name}-sepolia-aa-temp`)
  const described = awsJson('ecr', 'describe-repositories', ['--repository-names', ...repos]).repositories
  assert.equal(described.length, 3)
  for (const repository of described) assert.equal(repository.repositoryUri,
    `${imageTarget.registry}/${repository.repositoryName}`)

  process.umask(0o077)
  const workspace = mkdtempSync(join(tmpdir(), 'plether-frankfurt-images-'))
  const context = join(workspace, 'source')
  const auth = join(workspace, 'docker-auth')
  mkdirSync(context); mkdirSync(auth)
  const env = { ...process.env, DOCKER_CONFIG: auth }
  delete env.DOCKER_HOST; delete env.DOCKER_CONTEXT
  const docker = (args, options = {}) => execFileSync('docker', [...dockerArgs, ...args], { env, ...options })
  const archive = execFileSync('git', ['archive', imageTarget.sourceCommit, '.dockerignore',
    'apps/backend', 'apps/frontend/scripts/perps-oracle-worker.mjs',
    'apps/frontend/src/contracts/addresses.sepolia.json',
    'apps/frontend/src/contracts/addresses.arbitrum-sepolia.json',
    'apps/frontend/src/contracts/addresses.mainnet.json', 'config/perps/arbitrum-sepolia-v2.json'],
  { cwd: root, maxBuffer: 64 * 1024 * 1024 })
  execFileSync('tar', ['-xf', '-', '-C', context], { input: archive })
  const record = { ...imageTarget, coreRelease: 'v1.2.3', preparedAt: new Date().toISOString(), images: [] }
  const recordPath = join(workspace, 'images.json')
  console.log(`Private image workspace: ${workspace}`)
  try {
    const password = execFileSync('aws', ['--profile', 'plether', '--region', imageTarget.region,
      'ecr', 'get-login-password'], { encoding: 'utf8' })
    docker(['login', '--username', 'AWS', '--password-stdin', imageTarget.registry], { input: password, stdio: ['pipe', 'ignore', 'ignore'] })
    for (const name of ['alto', 'otel-log-router', 'api']) {
      const repository = `plether-${name}-sepolia-aa-temp`
      const tag = name === 'alto' ? imageTarget.altoTag : `source-${imageTarget.sourceCommit}`
      const reference = `${imageTarget.registry}/${repository}:${tag}`
      // Never overwrite an existing tag, including after a partial earlier run.
      const existing = awsJson('ecr', 'list-images', ['--repository-name', repository,
        '--filter', 'tagStatus=TAGGED']).imageIds.find(item => item.imageTag === tag)
      if (existing) {
        console.log(`Inspecting existing ${repository}:${tag}`)
        docker(['pull', '--platform', 'linux/arm64', `${imageTarget.registry}/${repository}@${existing.imageDigest}`], { stdio: 'inherit' })
        docker(['tag', `${imageTarget.registry}/${repository}@${existing.imageDigest}`, reference])
      } else if (name === 'alto') {
        docker(['pull', '--platform', 'linux/arm64', imageTarget.altoSource], { stdio: 'inherit' })
        docker(['tag', imageTarget.altoSource, reference])
      } else {
        const dockerfile = name === 'api' ? backendRecipe : join(context, 'apps/backend/otel-log-router/Dockerfile')
        docker(['buildx', 'build', '--pull', '--platform', 'linux/arm64', '--load', '--provenance=false',
          '--label', `org.opencontainers.image.revision=${imageTarget.sourceCommit}`,
          ...(name === 'api' ? ['--label', `com.plether.build-recipe.sha256=${imageTarget.backendRecipeSha256}`] : []),
          '--label', 'org.opencontainers.image.source=https://github.com/Plether-Fi/plether-app',
          '--file', dockerfile, '--tag', reference, context], { stdio: 'inherit' })
      }
      const inspected = JSON.parse(docker(['image', 'inspect', reference], { encoding: 'utf8' }))[0]
      assert.equal(inspected.Os, 'linux'); assert.equal(inspected.Architecture, 'arm64')
      if (name !== 'alto') assert.equal(inspected.Config.Labels['org.opencontainers.image.revision'], imageTarget.sourceCommit)
      if (name === 'api') {
        assert.equal(inspected.Config.Labels['com.plether.build-recipe.sha256'], imageTarget.backendRecipeSha256)
        docker(['run', '--rm', '--network', 'none', '--read-only', '--cap-drop', 'ALL',
          '--security-opt', 'no-new-privileges', '--pids-limit', '64', '--entrypoint', 'node', reference, '-e',
          'const fs=require("fs"),assert=require("assert"); const r=JSON.parse(fs.readFileSync("/app/config/perps/arbitrum-sepolia-v2.json")); assert.equal(r.release.version,"v1.2.3"); assert.equal(r.release.sourceCommit,"ffe45937b7f38133133ad292c5435828bf99357d"); for(const n of ["plether-api","plether-aa-reconciler","plether-aa-admin","plether-provider-preflight"])fs.accessSync("/usr/local/bin/"+n,fs.constants.X_OK);'], { stdio: 'inherit' })
      }
      if (!existing) docker(['push', reference], { stdio: 'inherit' })
      const response = awsJson('ecr', 'batch-get-image', ['--repository-name', repository, '--image-ids', `imageTag=${tag}`])
      assert.equal(response.failures.length, 0)
      const remote = response.images[0]
      const manifest = JSON.parse(remote.imageManifest)
      assert.equal(manifest.config.digest, inspected.Id, 'Published configuration differs from inspected image')
      if (name === 'alto') {
        const upstream = await approvedAltoManifest()
        assert.deepEqual(manifestIdentity(manifest), manifestIdentity(upstream), 'Alto config or ordered layer content differs from the approved upstream')
      }
      record.images.push({ name, repository, tag, digest: remote.imageId.imageDigest,
        reference: `${imageTarget.registry}/${repository}@${remote.imageId.imageDigest}`, platform: 'linux/arm64',
        ...(name === 'alto' ? { upstreamReference: imageTarget.altoSource, contentVerified: true } : {}) })
      writeFileSync(recordPath, JSON.stringify(record, null, 2) + '\n', { mode: 0o600 })
    }
    validateImageRecord(record)
    console.log(`Prepared and verified all three image digests: ${recordPath}. No services started.`)
  } finally {
    // This directory was created above exclusively for this run's ECR login.
    rmSync(auth, { recursive: true, force: true })
  }
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  main().catch(error => {
    console.error(`Image preparation stopped: ${error.code || error.message}`)
    process.exitCode = 1
  })
}
