#!/usr/bin/env node
import { randomBytes } from 'node:crypto'
import { generatePrivateKey, privateKeyToAccount } from '../apps/frontend/node_modules/viem/_esm/accounts/index.js'
import { verifyAccount, readSecret, createSecret, loadPreparedInputs, parseExecutorKeys, inputsParameter, namespace } from './frankfurt-aa-secrets.mjs'

async function prepare() {
  if (process.argv.length !== 3 || process.argv[2] !== '--prepare') throw new Error('Explicit --prepare required')
  verifyAccount()
  if (readSecret(inputsParameter) === null) {
    const rpc = readSecret('/plether/sepolia/rpc-url', 'ap-southeast-1')
    const perpsRpc = readSecret('/plether/sepolia/perps-rpc-url', 'ap-southeast-1')
    const posthogHeader = readSecret('/plether/sepolia/posthog-otlp-authorization-header', 'ap-southeast-1')
    for (const value of [rpc, perpsRpc]) {
      let url
      try { url = new URL(value) } catch { throw new Error('Invalid source RPC URL') }
      if (url.protocol !== 'https:' || !url.hostname.endsWith('.g.alchemy.com') || url.username || url.password) {
        throw new Error('Expected approved HTTPS Alchemy RPC inputs')
      }
    }
    if (!posthogHeader?.startsWith('Authorization Bearer phc_')) throw new Error('Unexpected PostHog authorization format')
    createSecret(inputsParameter, JSON.stringify({ schemaVersion: 1, deployment: 'sepolia-aa-temp', inputs: {
      rpc_url: rpc, perps_rpc_url: perpsRpc,
      keeper_private_key: generatePrivateKey(), oracle_updater_private_key: generatePrivateKey(),
      liquidation_keeper_private_key: generatePrivateKey(), db_password: randomBytes(32).toString('hex'),
      posthog_project_token: posthogHeader.slice('Authorization Bearer '.length),
      aa_proxy_origin_token: randomBytes(32).toString('hex'),
    } }))
  }
  const inputs = loadPreparedInputs()
  const external = [
    ['alto-executor-private-keys', () => Array.from({ length: 4 }, () => generatePrivateKey()).join(',')],
    ['alto-utility-private-key', () => generatePrivateKey()],
    ['pyth-api-key', () => {
      const value = readSecret('/plether/sepolia/pyth-api-key', 'ap-southeast-1')
      if (!value) throw new Error('Missing backend Pyth input')
      return value
    }],
  ]
  for (const [suffix, generate] of external) {
    const name = namespace + suffix
    if (readSecret(name) === null) createSecret(name, generate())
  }
  const executors = parseExecutorKeys(readSecret(namespace + 'alto-executor-private-keys'))
  const keys = [inputs.keeper_private_key, inputs.oracle_updater_private_key, inputs.liquidation_keeper_private_key,
    ...executors, readSecret(namespace + 'alto-utility-private-key')]
  if (new Set(keys).size !== keys.length || keys.some(key => !/^0x[0-9a-f]{64}$/.test(key) || /^0x0{64}$/.test(key))) {
    throw new Error('Invalid or reused service key')
  }
  const roles = ['keeper', 'oracle-updater', 'liquidation-worker', 'alto-executor-1',
    'alto-executor-2', 'alto-executor-3', 'alto-executor-4', 'alto-utility']
  // Only public addresses and parameter names leave this process.
  console.log(JSON.stringify({ inputsParameter, parameters: external.map(([suffix]) => namespace + suffix),
    wallets: keys.map((key, index) => ({ role: roles[index], address: privateKeyToAccount(key).address })),
    funding: 'No funding or transaction broadcast performed',
  }, null, 2))
}
prepare().catch(error => {
  // Avoid printing third-party exceptions that could include secret inputs.
  const safe = /^(AWS |Unexpected |Expected |Missing |Invalid |Explicit |Refusing |SecureString |Frankfurt )/.test(error.message)
  console.error(safe ? error.message : 'Secret preparation failed; response withheld to protect credentials')
  process.exitCode = 1
})
