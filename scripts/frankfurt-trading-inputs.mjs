import { randomBytes } from 'node:crypto'
import { generatePrivateKey, privateKeyToAccount } from '../apps/frontend/node_modules/viem/_esm/accounts/index.js'
import { readSecret, createSecret, verifyAccount } from './frankfurt-aa-secrets.mjs'

const parameter = '/plether/bootstrap/sepolia-aa-temp/trading-inputs'
const names = ['faucet_private_key', 'faucet_proxy_origin_token', 'lp_settlement_private_key', 'protection_worker_private_key']
export function loadTradingInputs() {
  const input = JSON.parse(readSecret(parameter) ?? 'null')
  if (!input || Object.keys(input).length !== names.length || names.some(n => !/^0x[0-9a-f]{64}$/.test(input[n] ?? '')))
    throw new Error('Invalid Frankfurt trading inputs')
  if (new Set(Object.values(input)).size !== names.length) throw new Error('Reused Frankfurt trading inputs')
  return input
}
if (process.argv[1]?.endsWith('/frankfurt-trading-inputs.mjs')) {
  try {
    if (process.argv[2] !== '--prepare') throw new Error('Explicit --prepare required')
    verifyAccount()
    if (readSecret(parameter) === null) createSecret(parameter, JSON.stringify({
      faucet_private_key: generatePrivateKey(), faucet_proxy_origin_token: `0x${randomBytes(32).toString('hex')}`,
      lp_settlement_private_key: generatePrivateKey(), protection_worker_private_key: generatePrivateKey(),
    }))
    const input = loadTradingInputs()
    console.log(JSON.stringify(Object.fromEntries(names.filter(n => n.endsWith('private_key')).map(n => [n.replace('_private_key', ''), privateKeyToAccount(input[n]).address]))))
  } catch { console.error('Trading input preparation failed; details withheld'); process.exitCode = 1 }
}
