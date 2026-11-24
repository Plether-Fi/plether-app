import { randomUUID } from 'node:crypto'
import { closeSync, fchmodSync, fsyncSync, linkSync, openSync, realpathSync, renameSync, unlinkSync, writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'

export function writePublicEvidence(path, contents, exclusive = false) {
  const target = resolve(path)
  const temporary = `${target}.${randomUUID()}.tmp`
  const fd = openSync(temporary, 'wx', 0o600)
  try {
    try {
      writeFileSync(fd, contents)
      fchmodSync(fd, 0o600)
      fsyncSync(fd)
    } finally {
      closeSync(fd)
    }
    if (exclusive) {
      linkSync(temporary, target)
      unlinkSync(temporary)
    } else {
      renameSync(temporary, target)
    }
    const directory = openSync(dirname(target), 'r')
    try { fsyncSync(directory) } finally { closeSync(directory) }
  } catch (error) {
    try { unlinkSync(temporary) } catch { /* A published file remains available. */ }
    throw error
  }
}

export function isCliEntry(metaUrl, scriptPath = process.argv[1]) {
  if (!scriptPath) return false
  try {
    return pathToFileURL(realpathSync(scriptPath)).href === pathToFileURL(realpathSync(fileURLToPath(metaUrl))).href
  } catch (error) {
    if (error?.code === 'ENOENT') return false
    throw error
  }
}

export function scrubRpcError(error, secrets = []) {
  let message = String(error?.shortMessage ?? error?.message ?? error)
  for (const secret of secrets) {
    if (!secret) continue
    if (/^(?:0x)?[0-9a-f]{64}$/iu.test(secret)) {
      message = message.replace(new RegExp(`(?:0x)?${secret.replace(/^0x/iu, '')}`, 'giu'), '[REDACTED]')
    } else {
      message = message.replaceAll(secret, '[REDACTED]')
    }
  }
  return message.replace(/https?:\/\/[^\s)]+/giu, '[RPC]')
}
