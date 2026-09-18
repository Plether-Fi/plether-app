import { lstat, readdir, unlink } from 'node:fs/promises'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

// Generated build output only. Do not follow symlinks or accept arbitrary paths.
export async function removeSourceMaps(directory) {
  if (!(await lstat(directory)).isDirectory()) throw new Error('Expected a directory, not a symlink')
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const filename = path.join(directory, entry.name)
    if (entry.isSymbolicLink()) throw new Error('Symlink in publish payload')
    if (entry.isDirectory()) await removeSourceMaps(filename)
    else if (/\.map$/i.test(entry.name)) await unlink(filename)
  }
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  if (process.argv.length !== 3 || process.argv[2] !== 'dist') throw new Error('Expected the frontend dist directory')
  const dist = fileURLToPath(new URL('../dist', import.meta.url))
  await removeSourceMaps(dist).catch(error => {
    // A failed build may not have created dist. Other failures block publishing.
    if (error.code !== 'ENOENT') throw error
  })
}
