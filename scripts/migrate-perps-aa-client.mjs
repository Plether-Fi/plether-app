import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const frontend = path.join(root, 'apps/frontend')
const legacyName = '@plether/perps-aa-client'
export const packageName = '@plether-fi/perps-aa-client'
const registry = 'https://npm.pkg.github.com'
const repository = 'Plether-Fi/plether-core'
const run = (command, args, cwd = root) => execFileSync(command, args, { cwd, encoding: 'utf8' }).trim()
const readJson = file => JSON.parse(readFileSync(file, 'utf8'))
const saveJson = (file, value) => writeFileSync(file, JSON.stringify(value, null, 2) + '\n')
const ghJson = args => JSON.parse(run('gh', args))
const api = endpoint => ghJson(['api', endpoint])

export function validatePublishedRelease(metadata, expectedSha, version, tagCommit, visibility) {
  assert.match(version, /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)$/)
  assert.equal(version.trim(), version)
  assert.match(expectedSha, /^[a-f0-9]{40}$/)
  assert.equal(expectedSha.trim(), expectedSha)
  assert.equal(metadata.name, packageName)
  assert.equal(metadata.version, version)
  assert.equal(metadata.gitHead, expectedSha, 'Registry package comes from another Core commit')
  assert.equal(tagCommit, expectedSha, 'Core package tag does not match the registry commit')
  assert.equal(visibility, 'public', 'Verify public package visibility first')
  assert.match(metadata.dist?.integrity ?? '', /^sha512-[A-Za-z0-9+/]+={0,2}$/)
  const tarball = new URL(metadata.dist.tarball)
  assert.equal(tarball.origin, registry, 'Unexpected package registry')
  assert.equal(tarball.username + tarball.password + tarball.search + tarball.hash, '', 'Unexpected credentials or query in tarball URL')
  return { package: packageName, version, sourceRepository: repository, sourceCommit: expectedSha,
    tag: `perps-aa-client-v${version}`, integrity: metadata.dist.integrity, tarball: metadata.dist.tarball }
}

export function replaceSection(text, start, end, replacement) {
  assert.equal(text.split(start).length, 2, `Expected one migration start: ${start}`)
  assert.equal(text.split(end).length, 2, `Expected one migration end: ${end}`)
  const from = text.indexOf(start)
  const to = text.indexOf(end, from)
  assert.ok(to > from)
  return text.slice(0, from) + replacement.trimEnd() + '\n\n' + text.slice(to)
}

export function removeLegacyLockEntries(lock) {
  assert.equal(lock.lockfileVersion, 3)
  assert.equal(lock.packages[''].dependencies[legacyName], undefined, 'Remove the legacy dependency before pruning its lock entry')
  // npm can retain an orphan file-dependency entry even after replacing its
  // root dependency. It must not survive as misleading vendor provenance.
  delete lock.packages['vendor/perps-aa-client']
  delete lock.packages[`node_modules/${legacyName}`]
  assert.ok(!JSON.stringify(lock).includes(legacyName))
  assert.ok(!JSON.stringify(lock).includes('vendor/perps-aa-client'))
  return lock
}

export function protectionWorkerProvenance(release) {
  assert.match(release.version, /^v\d+\.\d+\.\d+$/)
  assert.match(release.sourceCommit, /^[a-f0-9]{40}$/)
  assert.match(release.bundleSha256, /^[a-f0-9]{64}$/)
  return `The protection-worker ABI is generated separately from the deployed Core release bundle:\n\n\`\`\`bash\nnode scripts/generate-protection-worker-abi.mjs /path/to/perps-${release.version}-arbitrum-sepolia.tar.gz\n\`\`\`\n\nThe generator verifies the bundle SHA-256 against\n\`config/perps/arbitrum-sepolia-v2.json\` before extracting the\nPositionProtectionBook, OrderRouter, OrderLifecycleBook and PletherOracle ABIs.\nDeployment source: \`${release.sourceCommit}\`.\nBundle SHA-256: \`${release.bundleSha256}\`.\nThe AA package release is independent of this deployment-specific ABI bundle;\npackage migration does not regenerate worker artifacts.`
}

export function migrateSources(directory, record) {
  const { release } = readJson(path.join(directory, 'config/perps/arbitrum-sepolia-v2.json'))
  const edit = (file, change) => {
    const target = path.join(directory, file)
    writeFileSync(target, change(readFileSync(target, 'utf8')))
  }
  const sourceFiles = run('git', ['ls-files', '-z', 'apps/frontend/src'], directory).split('\0').filter(file => /\.[cm]?[jt]sx?$/.test(file))
  for (const file of sourceFiles) edit(file, text => text.replaceAll(legacyName, packageName))
  const sourceUrl = `https://github.com/${repository}/blob/${record.sourceCommit}/packages/perps-aa-client`
  const releaseUrl = `https://github.com/${repository}/releases/tag/${record.tag}`
  edit('docs/runbooks/self-hosted-aa-rollout.md', text => replaceSection(text,
    'Until the AA package is published and adopted,', '### GitHub and AWS preflight',
    `The AA contracts and client come from [Core PR](${record.corePr}) and the\n[immutable package release](${releaseUrl}). The frontend consumes\n\`${packageName}@${record.version}\`; the lockfile records its registry integrity.\n\nCore source commit: \`${record.sourceCommit}\`.\nPackage integrity: \`${record.integrity}\`.\nThe machine-readable provenance is \`config/perps-aa-client-release.json\`.\nBuild and deploy the paymaster only from this reviewed clean Core commit.\nPackage publication is separate from contract deployment and sponsorship enablement.`))
  edit('docs/runbooks/position-protection.md', text => replaceSection(text,
    '## Build provenance', '## Worker and history',
    `## Build provenance\n\nThe Book ABI and all four protection action builders are published with native\nsponsorship support in [${packageName}@${record.version}](${releaseUrl}).\nSource: [reviewed Core commit](${sourceUrl}), PR: ${record.corePr}.\n\`config/perps-aa-client-release.json\` and the frontend lockfile record the\npackage commit, version, tag, and integrity. Upgrade through a reviewed immutable\npackage release; never edit installed package files.\n\n${protectionWorkerProvenance(release)}`))
  edit('apps/gitbook/DIAGRAM_REVIEW.md', text => {
    const original = 'Account-abstraction ordering was checked against the vendored client’s `sendSponsoredAction`, the frontend operation-status mapping, withdrawal action encoding, and receipt-recovery code. The vendored client’s provenance is recorded in `apps/frontend/vendor/perps-aa-client/UPSTREAM.md`.'
    assert.ok(text.includes(original), 'Review changed GitBook provenance before migrating')
    return text.replace(original, `Account-abstraction ordering was checked against [${packageName}@${record.version}](${sourceUrl}/src/orchestrator.ts), the frontend operation-status mapping, withdrawal action encoding, and receipt-recovery code. Package provenance is recorded in \`config/perps-aa-client-release.json\`.`)
  })
  edit('apps/gitbook/scripts/diagram-catalog.mjs', text => text
    .replaceAll('apps/frontend/vendor/perps-aa-client/dist/actions.js', `${sourceUrl}/src/actions.ts`)
    .replaceAll('apps/frontend/vendor/perps-aa-client/dist/orchestrator.js:sendSponsoredAction', `${sourceUrl}/src/orchestrator.ts`))
  saveJson(path.join(directory, 'config/perps-aa-client-release.json'), record)
}

async function main() {
  const [version, expectedSha, corePrNumber] = process.argv.slice(2)
  assert.match(corePrNumber ?? '', /^[1-9]\d*$/, 'Usage: node scripts/migrate-perps-aa-client.mjs VERSION CORE_SHA CORE_PR_NUMBER')
  assert.equal(run('git', ['status', '--porcelain']), '', 'Commit or preserve local edits before migration')
  // Complete registry and upstream checks before changing any tracked file.
  const metadata = JSON.parse(run('npm', ['view', `${packageName}@${version}`, '--json', '--registry', registry], frontend))
  const tag = api(`repos/${repository}/git/ref/tags/perps-aa-client-v${version}`)
  const target = tag.object.type === 'tag' ? api(`repos/${repository}/git/tags/${tag.object.sha}`).object : tag.object
  assert.equal(target.type, 'commit')
  const packages = ghJson(['api', '--paginate', '--slurp', 'orgs/Plether-Fi/packages?package_type=npm&per_page=100']).flat()
  const pkg = packages.find(item => item.name === packageName || item.name === 'perps-aa-client')
  assert.ok(pkg, 'Published package must be visible to the authenticated GitHub user')
  assert.equal(pkg.repository?.full_name, repository)
  const record = validatePublishedRelease(metadata, expectedSha, version, target.sha, pkg.visibility)
  const pr = ghJson(['pr', 'view', corePrNumber, '--repo', repository, '--json', 'state,mergeCommit,url'])
  assert.equal(pr.state, 'MERGED', 'Merge the reviewed Core PR first')
  for (const comparison of [`${pr.mergeCommit.oid}...${expectedSha}`, `${expectedSha}...master`]) {
    assert.ok(['ahead', 'identical'].includes(api(`repos/${repository}/compare/${comparison}`).status), 'Package source must include the Core PR and belong to master')
  }
  const release = api(`repos/${repository}/releases/tags/${record.tag}`)
  assert.equal(release.draft, false)
  assert.equal(release.prerelease, false)
  assert.ok(release.body.includes(expectedSha) && release.body.includes(record.integrity), 'Release record must identify the same artifact')
  record.corePr = pr.url
  const scratch = mkdtempSync(path.join(tmpdir(), 'perps-aa-registry-check-'))
  const [packed] = JSON.parse(run('npm', ['pack', `${packageName}@${version}`, '--json', '--ignore-scripts', '--pack-destination', scratch, '--registry', registry], frontend))
  const tarball = path.join(scratch, path.basename(packed.filename))
  assert.equal(`sha512-${createHash('sha512').update(readFileSync(tarball)).digest('base64')}`, record.integrity)
  saveJson(path.join(scratch, 'package.json'), { private: true, dependencies: { [packageName]: version } })
  writeFileSync(path.join(scratch, '.npmrc'), `@plether-fi:registry=${registry}\n`)
  run('npm', ['install', '--ignore-scripts', '--no-audit', '--no-fund'], scratch)
  run('npm', ['ci', '--ignore-scripts', '--no-audit', '--no-fund'], scratch)

  const manifestPath = path.join(frontend, 'package.json')
  const manifest = readJson(manifestPath)
  assert.equal(manifest.dependencies[legacyName], 'file:vendor/perps-aa-client')
  delete manifest.dependencies[legacyName]
  manifest.dependencies[packageName] = version
  saveJson(manifestPath, manifest)
  run('npm', ['install', '--package-lock-only', '--ignore-scripts', '--no-audit', '--no-fund'], frontend)
  const lock = readJson(path.join(frontend, 'package-lock.json'))
  const locked = lock.packages[`node_modules/${packageName}`]
  assert.equal(locked.version, version)
  assert.equal(locked.resolved, record.tarball)
  assert.equal(locked.integrity, record.integrity)
  assert.ok(!locked.link)
  saveJson(path.join(frontend, 'package-lock.json'), removeLegacyLockEntries(lock))
  migrateSources(root, record)
  run('npm', ['ci', '--ignore-scripts', '--no-audit', '--no-fund'], frontend)
  // Fixed repository-relative targets only. Git retains all removed artifacts.
  for (const file of ['.codex-artifacts/plether-core-self-hosted-aa.patch', '.codex-artifacts/plether-core-self-hosted-aa.manifest.txt', 'scripts/vendor-perps-aa-client.mjs']) rmSync(path.join(root, file))
  rmSync(path.join(frontend, 'vendor/perps-aa-client'), { recursive: true })
  run(process.execPath, ['apps/gitbook/scripts/generate-diagrams.mjs'])
  console.log(`Migration prepared from ${record.corePr}. Review git diff, run frontend/AA/Storybook checks, and verify the PR's packages:read installation before merging.`)
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) await main()
