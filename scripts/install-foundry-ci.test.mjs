import { strict as assert } from 'node:assert'
import { createHash } from 'node:crypto'
import { existsSync, mkdtempSync, readFileSync, readdirSync, rmSync, symlinkSync, writeFileSync, writeSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { basename, join } from 'node:path'
import { test } from 'node:test'
import { installFoundry, release } from './install-foundry-ci.mjs'

function fixture(t, { corrupt = false, reject = null, symlink = false } = {}) {
  const directory = mkdtempSync(join(tmpdir(), 'foundry-fallback-test-'))
  t.after(() => { rmSync(directory, { recursive: true, force: true }) })
  const githubPath = join(directory, 'github-path')
  writeFileSync(githubPath, 'existing-path\n')
  const archive = Buffer.from('Synthetic archive; no executable tools or network calls.')
  const verified = []
  const run = (command, args, options) => {
    if (args[0] === 'api') {
      assert.equal(command, 'gh')
      assert.deepEqual(args, ['api', '-H', 'Accept: application/octet-stream', 'repos/foundry-rs/foundry/releases/assets/330667427'])
      writeSync(options.stdio[1], corrupt ? Buffer.from('corrupt download') : archive)
    } else if (command === 'tar') {
      assert.deepEqual(args.slice(4), ['forge', 'cast', 'anvil', 'chisel'])
      for (const tool of args.slice(4)) writeFileSync(join(args[3], tool), `synthetic ${tool}`)
      if (symlink) {
        rmSync(join(args[3], 'chisel'))
        symlinkSync(join(args[3], 'forge'), join(args[3], 'chisel'))
      }
    } else {
      assert.equal(command, 'gh')
      assert.deepEqual(args.slice(0, 2), ['attestation', 'verify'])
      assert.deepEqual(args.slice(3), ['--repo', 'foundry-rs/foundry', '--source-ref', 'refs/tags/v1.5.1'])
      assert.equal(readFileSync(githubPath, 'utf8'), 'existing-path\n')
      const tool = basename(args[2])
      if (tool === reject) throw new Error('Invalid signed provenance')
      verified.push(tool)
    }
  }
  return {
    options: { run, directory, githubPath, platform: 'linux', arch: 'x64', pinned: { ...release, sha256: createHash('sha256').update(archive).digest('hex') } },
    directory, githubPath, verified,
  }
}

test('publishes the pinned tools only after every provenance check succeeds', t => {
  const input = fixture(t)
  const bin = installFoundry(input.options)
  assert.deepEqual(input.verified, ['forge', 'cast', 'anvil', 'chisel'])
  assert.equal(readFileSync(input.githubPath, 'utf8'), `existing-path\n${bin}\n`)
  assert.equal(existsSync(join(bin, '..', 'foundry.tar.gz')), false)
})

test('rejects a corrupt archive before extraction or verification', t => {
  const input = fixture(t, { corrupt: true })
  assert.throws(() => installFoundry(input.options), /checksum/)
  assert.deepEqual(input.verified, [])
  assert.deepEqual(readdirSync(input.directory), ['github-path'])
  assert.equal(readFileSync(input.githubPath, 'utf8'), 'existing-path\n')
})

test('a failed provenance check prevents installation of the entire release', t => {
  const input = fixture(t, { reject: 'chisel' })
  assert.throws(() => installFoundry(input.options), /Invalid signed provenance/)
  assert.deepEqual(readdirSync(input.directory), ['github-path'])
  assert.equal(readFileSync(input.githubPath, 'utf8'), 'existing-path\n')
})

test('rejects symbolic links without publishing the tools', t => {
  const input = fixture(t, { symlink: true })
  assert.throws(() => installFoundry(input.options), /not a regular file/)
  assert.equal(readFileSync(input.githubPath, 'utf8'), 'existing-path\n')
})

test('stops before downloading on unsupported platforms', t => {
  const input = fixture(t)
  assert.throws(() => installFoundry({ ...input.options, platform: 'darwin' }), /Linux/)
  assert.deepEqual(readdirSync(input.directory), ['github-path'])
})
