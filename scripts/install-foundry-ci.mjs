import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { appendFileSync, closeSync, lstatSync, mkdirSync, mkdtempSync, openSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { pathToFileURL } from 'node:url'

// Official v1.5.1 Linux/amd64 release. The asset API serves the same archive
// when GitHub's public release-download endpoints return HTTP 504.
export const release = Object.freeze({
  version: 'v1.5.1',
  assetId: 330667427,
  sha256: '73640b01bd9ed29fdb4965085099371f8cf0dbbec3e2086cf54564efc4dcfe88',
})
const tools = ['forge', 'cast', 'anvil', 'chisel']

export function installFoundry({
  run = execFileSync,
  githubPath = process.env.GITHUB_PATH,
  directory = tmpdir(),
  platform = process.platform,
  arch = process.arch,
  pinned = release,
} = {}) {
  if (platform !== 'linux' || arch !== 'x64') throw new Error('Foundry CI fallback supports Linux/amd64 only')
  if (!githubPath) throw new Error('GITHUB_PATH is required')
  const staging = mkdtempSync(join(directory, 'plether-foundry-'))
  const archive = join(staging, 'foundry.tar.gz')
  const bin = join(staging, 'bin')
  try {
    const output = openSync(archive, 'w', 0o600)
    try {
      run('gh', ['api', '-H', 'Accept: application/octet-stream', `repos/foundry-rs/foundry/releases/assets/${pinned.assetId}`], {
        timeout: 180_000,
        stdio: ['ignore', output, 'inherit'],
      })
    } finally { closeSync(output) }
    const digest = createHash('sha256').update(readFileSync(archive)).digest('hex')
    if (digest !== pinned.sha256) throw new Error('Foundry archive checksum does not match the pinned release')
    mkdirSync(bin)
    run('tar', ['-xzf', archive, '-C', bin, ...tools], { timeout: 180_000, stdio: 'inherit' })
    for (const tool of tools) {
      const binary = join(bin, tool)
      if (!lstatSync(binary).isFile()) throw new Error(`Foundry ${tool} is not a regular file`)
      run('gh', ['attestation', 'verify', binary, '--repo', 'foundry-rs/foundry', '--source-ref', `refs/tags/${pinned.version}`], {
        timeout: 180_000,
        stdio: 'inherit',
      })
    }
    // Publish the tools only after every binary's signed provenance is verified.
    appendFileSync(githubPath, `${bin}\n`)
    rmSync(archive)
    return bin
  } catch (error) {
    rmSync(staging, { recursive: true, force: true })
    throw error
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  console.log(`Installing verified Foundry ${release.version} through the GitHub API`)
  installFoundry()
}
