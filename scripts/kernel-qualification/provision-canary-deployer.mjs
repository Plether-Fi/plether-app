#!/usr/bin/env node
// One-off isolated Sepolia deployer. The private key goes directly to SSM through stdin.
import assert from 'node:assert/strict'
import { isCliEntry } from './local-safety.mjs'
import { createRequire } from 'node:module'
import { parseArgs } from 'node:util'
import { awsCliClient } from './provision-canary-credentials.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { generatePrivateKey, privateKeyToAccount } = require('viem/accounts')

export const PARAMETER_NAME = '/plether/sepolia/kernel-canary-deployer-private-key'
export const CONFIRMATION = 'kernel-sepolia-dedicated-deployer'
export const CANARY_SSM_KMS_ARN = 'arn:aws:kms:ap-southeast-1:932542905614:key/65a4abd7-44fb-46f3-821d-71dab7d4efe3'
const ACCOUNT = '932542905614'
const REGION = 'ap-southeast-1'
const EXISTING_DEPLOYER = '0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b'

function usage() {
  return `Usage:
  node scripts/kernel-qualification/provision-canary-deployer.mjs --region ${REGION} --kms-key-arn ARN
  node scripts/kernel-qualification/provision-canary-deployer.mjs --region ${REGION} --kms-key-arn ARN --apply --confirm ${CONFIRMATION}

Default: check AWS identity, the dedicated symmetric KMS key, and parameter absence. No key is generated.
Apply: generate one new key in memory, store a non-overwriting SSM SecureString, and print only its public address.`
}

export function parseOptions(argv) {
  const { values, positionals } = parseArgs({
    args: argv,
    options: {
      region: { type: 'string' }, 'kms-key-arn': { type: 'string' },
      apply: { type: 'boolean' }, confirm: { type: 'string' }, help: { type: 'boolean' },
    },
    allowPositionals: true,
    strict: true,
  })
  if (values.help) return { help: true }
  assert.equal(positionals.length, 0, 'unexpected positional arguments')
  assert.equal(values.region, REGION, `canary deployer region must be ${REGION}`)
  assert.equal(values['kms-key-arn'], CANARY_SSM_KMS_ARN, 'pass the exact dedicated canary symmetric KMS key ARN')
  assert.ok(!values.confirm || values.apply, '--confirm is valid only with --apply')
  if (values.apply) assert.equal(values.confirm, CONFIRMATION, 'explicit dedicated deployer confirmation is required')
  return { mode: values.apply ? 'apply' : 'plan', region: REGION, kmsKeyArn: values['kms-key-arn'] }
}

export async function execute(options, client, generate = generatePrivateKey) {
  const identity = await client.identity()
  assert.equal(identity.Account, ACCOUNT, 'AWS profile is not the reviewed Plether account')
  assert.ok(identity.Arn?.includes(`:${ACCOUNT}:`), 'AWS caller identity is inconsistent')
  assert.equal(options.kmsKeyArn, CANARY_SSM_KMS_ARN, 'canary SSM key ARN changed')
  assert.equal(options.region, REGION, 'canary deployer region changed')
  const key = await client.keyMetadata(options.kmsKeyArn)
  assert.deepEqual({
    Arn: key.Arn, KeyManager: key.KeyManager, KeyState: key.KeyState,
    KeySpec: key.KeySpec, KeyUsage: key.KeyUsage,
  }, {
    Arn: options.kmsKeyArn, KeyManager: 'CUSTOMER', KeyState: 'Enabled',
    KeySpec: 'SYMMETRIC_DEFAULT', KeyUsage: 'ENCRYPT_DECRYPT',
  }, 'canary SSM key metadata changed')
  assert.equal(await client.parameterExists(PARAMETER_NAME), false,
    `dedicated canary deployer parameter already exists: ${PARAMETER_NAME}`)
  if (options.mode === 'plan') {
    return { mode: 'plan', account: ACCOUNT, region: REGION, kmsKeyArn: options.kmsKeyArn,
      absentParameter: PARAMETER_NAME }
  }
  assert.equal(options.mode, 'apply', 'unknown provisioning mode')
  const privateKey = generate()
  assert.match(privateKey ?? '', /^0x[0-9a-f]{64}$/u, 'generated deployer key has an invalid format')
  let deployerAddress
  try { deployerAddress = privateKeyToAccount(privateKey).address }
  catch { throw new Error('generated deployer key is outside the secp256k1 range') }
  assert.notEqual(deployerAddress.toLowerCase(), EXISTING_DEPLOYER,
    'dedicated canary deployer must differ from the active Sepolia order signer')
  try { await client.putSecureString(PARAMETER_NAME, privateKey, options.kmsKeyArn) }
  catch { throw new Error(`dedicated deployer secret write may have reached SSM; inspect ${PARAMETER_NAME} before retrying`) }
  return { mode: 'applied', account: ACCOUNT, region: REGION, kmsKeyArn: options.kmsKeyArn,
    parameterName: PARAMETER_NAME, deployerAddress }
}

async function main() {
  const options = parseOptions(process.argv.slice(2))
  if (options.help) { console.log(usage()); return }
  console.log(JSON.stringify(await execute(options, awsCliClient(options.region)), null, 2))
}

if (isCliEntry(import.meta.url)) {
  main().catch(error => { console.error(`Canary deployer provisioning failed: ${error.message}`); process.exitCode = 1 })
}
