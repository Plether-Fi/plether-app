import assert from 'node:assert/strict'
import { mkdtemp, mkdir, writeFile, readdir, rm, symlink } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { test } from 'node:test'
import { removeSourceMaps } from './remove-source-maps.mjs'

test('removes maps recursively while retaining executable assets', async () => {
  const root = await mkdtemp(path.join(tmpdir(), 'plether-source-maps-'))
  try {
    await mkdir(path.join(root, 'assets'))
    for (const name of ['app.js', 'app.js.map', 'app.css', 'app.css.map']) await writeFile(path.join(root, 'assets', name), 'fixture')
    await removeSourceMaps(root)
    assert.deepEqual((await readdir(path.join(root, 'assets'))).sort(), ['app.css', 'app.js'])
    await removeSourceMaps(root) // uploader may already have removed every map
    await symlink(root, path.join(root, 'link'))
    await assert.rejects(removeSourceMaps(root), /Symlink/)
  } finally {
    await rm(root, { recursive: true, force: true })
  }
})
