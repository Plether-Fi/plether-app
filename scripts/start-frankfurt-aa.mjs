#!/usr/bin/env node
// Explicit local-only canary launcher. Origin token never enters files or argv.
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { verifyAccount, readSecret } from './frankfurt-aa-secrets.mjs'

try {
  if (process.argv.length !== 3) throw new Error('Usage: node scripts/start-frankfurt-aa.mjs <reviewed-outputs.json>')
  verifyAccount()
  const token = readSecret('/plether/sepolia-aa-temp/aa-proxy-origin-token')
  if (!/^[0-9a-f]{64}$/.test(token ?? '')) throw new Error('Invalid Frankfurt origin credential')
  const faucetToken = readSecret('/plether/sepolia-aa-temp/faucet-proxy-origin-token')
  const child = spawn(process.execPath, ['apps/frontend/node_modules/vite/bin/vite.js',
    '--config', 'scripts/aa-frankfurt.vite.config.mts'], {
    cwd: fileURLToPath(new URL('../', import.meta.url)), stdio: 'inherit',
    env: { ...process.env, AA_FRANKFURT_MODE: 'aa', AA_PROXY_ORIGIN_TOKEN: token,
      ...(faucetToken ? { FAUCET_PROXY_ORIGIN_TOKEN: faucetToken } : {}),
      AA_FRANKFURT_OUTPUTS_FILE: process.argv[2],
      AA_FRANKFURT_PAYMASTER_ADDRESS: '0x9761091045616A388f5fE1433721B272c78fe31b',
      AA_FRANKFURT_SPONSORSHIP_ENABLED: 'true' },
  })
  child.once('error', () => { console.error('Could not start local Vite'); process.exitCode = 1 })
  child.once('exit', code => { process.exitCode = code ?? 1 })
  for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => child.kill(signal))
} catch {
  console.error('Frankfurt local launcher refused configuration; details withheld')
  process.exitCode = 1
}
