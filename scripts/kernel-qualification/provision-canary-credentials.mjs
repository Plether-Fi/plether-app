#!/usr/bin/env node
// Generates fresh canary credentials only after the read-only preflight and an explicit apply gate.
import assert from 'node:assert/strict'
import { randomBytes } from 'node:crypto'
import { spawn } from 'node:child_process'
import { createRequire } from 'node:module'
import { parseArgs } from 'node:util'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { getAddress, keccak256 } = require('viem')
const { generatePrivateKey, privateKeyToAccount } = require('viem/accounts')

export const PARAMETER_NAMES = Object.freeze({
  executor: '/plether/sepolia/kernel-canary-alto-executor-private-keys',
  utility: '/plether/sepolia/kernel-canary-alto-utility-private-key',
  origin: '/plether/sepolia/kernel-canary-aa-origin-token',
})
const CONFIRMATION = 'kernel-sepolia-canary-secrets'
const SPKI_PREFIX = Buffer.from('3056301006072a8648ce3d020106052b8104000a034200', 'hex')
const KMS_ARN = /^arn:aws:kms:([a-z0-9-]+):(\d{12}):key\/([0-9a-f-]{36})$/

function usage() {
  return `Usage:
  node scripts/kernel-qualification/provision-canary-credentials.mjs --region ap-southeast-1 --kms-key-arn ARN
  node scripts/kernel-qualification/provision-canary-credentials.mjs --region ap-southeast-1 --kms-key-arn ARN --apply --confirm ${CONFIRMATION}
  node scripts/kernel-qualification/provision-canary-credentials.mjs --region ap-southeast-1 --sponsor-key-arn ARN

Default mode checks account, symmetric KMS key and parameter absence without generating credentials.
Apply writes three new SSM SecureStrings, never overwrites, and prints public metadata only.
Sponsor mode reads the asymmetric KMS public key and prints its Ethereum address without signing.`
}

export function parseOptions(argv) {
  const { values, positionals } = parseArgs({
    args: argv,
    options: {
      region: { type: 'string' },
      'kms-key-arn': { type: 'string' },
      'sponsor-key-arn': { type: 'string' },
      apply: { type: 'boolean' },
      confirm: { type: 'string' },
      help: { type: 'boolean' },
    },
    allowPositionals: true,
    strict: true,
  })
  if (values.help) return { help: true }
  assert.equal(positionals.length, 0, 'unexpected positional arguments')
  assert.match(values.region ?? '', /^[a-z]{2}-[a-z]+-\d$/, '--region is required')
  if (values['sponsor-key-arn']) {
    assert.ok(!values.apply && !values.confirm && !values['kms-key-arn'], 'sponsor address mode is read-only and separate from provisioning')
    return { mode: 'sponsor', region: values.region, sponsorKeyArn: values['sponsor-key-arn'] }
  }
  assert.ok(values['kms-key-arn'], '--kms-key-arn is required for credential provisioning')
  assert.ok(!values.confirm || values.apply, '--confirm is valid only with --apply')
  if (values.apply) assert.equal(values.confirm, CONFIRMATION, 'explicit canary secret confirmation is required')
  return { mode: values.apply ? 'apply' : 'plan', region: values.region, kmsKeyArn: values['kms-key-arn'] }
}

function checkKeyArn(arn, region, account) {
  const match = KMS_ARN.exec(arn ?? '')
  assert.ok(match, 'KMS key must be an explicit key ARN')
  assert.equal(match[1], region, 'KMS key region does not match --region')
  assert.equal(match[2], account, 'KMS key account does not match the plether profile')
}

function checkSymmetricKey(metadata, arn) {
  assert.equal(metadata.Arn, arn, 'KMS returned a different key ARN')
  assert.equal(metadata.KeyManager, 'CUSTOMER', 'a customer-managed KMS key is required')
  assert.equal(metadata.KeyState, 'Enabled', 'KMS key must be enabled')
  assert.equal(metadata.KeySpec, 'SYMMETRIC_DEFAULT', 'SSM SecureString requires a symmetric KMS key')
  assert.equal(metadata.KeyUsage, 'ENCRYPT_DECRYPT', 'SSM SecureString requires an encryption KMS key')
}

function checkSponsorKey(metadata, arn) {
  assert.equal(metadata.Arn, arn, 'KMS returned a different sponsor key ARN')
  assert.equal(metadata.KeyState, 'Enabled', 'sponsor key must be enabled')
  assert.equal(metadata.KeySpec, 'ECC_SECG_P256K1', 'sponsor key must use secp256k1')
  assert.equal(metadata.KeyUsage, 'SIGN_VERIFY', 'sponsor key must be able to sign')
}

export function kmsPublicKeyToAddress(derBase64) {
  assert.match(derBase64 ?? '', /^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$/, 'invalid KMS public key base64')
  const der = Buffer.from(derBase64, 'base64')
  assert.equal(der.length, SPKI_PREFIX.length + 65, 'unexpected KMS public key length')
  assert.ok(der.subarray(0, SPKI_PREFIX.length).equals(SPKI_PREFIX), 'noncanonical KMS secp256k1 SPKI')
  assert.equal(der[SPKI_PREFIX.length], 0x04, 'KMS public key is not uncompressed')
  const hash = keccak256(`0x${der.subarray(SPKI_PREFIX.length + 1).toString('hex')}`)
  return getAddress(`0x${hash.slice(-40)}`)
}

function generateCredentials() {
  const executor = generatePrivateKey()
  let utility = generatePrivateKey()
  while (utility === executor) utility = generatePrivateKey()
  const origin = randomBytes(32).toString('hex')
  return { executor, utility, origin }
}

export async function execute(options, client, generate = generateCredentials) {
  const identity = await client.identity()
  const account = identity.Account
  assert.match(account ?? '', /^\d{12}$/, 'AWS caller account is invalid')
  assert.ok(identity.Arn?.includes(`:${account}:`), 'AWS caller identity is inconsistent')
  if (options.mode === 'sponsor') {
    checkKeyArn(options.sponsorKeyArn, options.region, account)
    const metadata = await client.keyMetadata(options.sponsorKeyArn)
    checkSponsorKey(metadata, options.sponsorKeyArn)
    const publicKey = await client.publicKey(options.sponsorKeyArn)
    assert.deepEqual(publicKey.KeySpec, 'ECC_SECG_P256K1')
    assert.deepEqual(publicKey.KeyUsage, 'SIGN_VERIFY')
    return { mode: 'sponsor', account, region: options.region, keyArn: options.sponsorKeyArn, sponsorSigner: kmsPublicKeyToAddress(publicKey.PublicKey) }
  }
  checkKeyArn(options.kmsKeyArn, options.region, account)
  checkSymmetricKey(await client.keyMetadata(options.kmsKeyArn), options.kmsKeyArn)
  const names = Object.values(PARAMETER_NAMES)
  // Do every read before generating credentials or making any write.
  const present = await Promise.all(names.map(name => client.parameterExists(name)))
  assert.ok(present.every(value => value === false), `canary SSM parameter already exists: ${names.filter((_, index) => present[index]).join(', ')}`)
  if (options.mode === 'plan') {
    return { mode: 'plan', account, region: options.region, kmsKeyArn: options.kmsKeyArn, absentParameters: names }
  }
  assert.equal(options.mode, 'apply')
  const secrets = generate()
  if (!/^0x[0-9a-f]{64}$/.test(secrets.executor) || !/^0x[0-9a-f]{64}$/.test(secrets.utility)) {
    throw new Error('generated Alto key has an invalid format')
  }
  if (secrets.executor === secrets.utility) throw new Error('canary Alto signers must be distinct')
  if (!/^[0-9a-f]{64}$/.test(secrets.origin) || secrets.origin === '0'.repeat(64)) {
    throw new Error('generated origin token is invalid')
  }
  let executorAddress, utilityAddress
  try {
    executorAddress = privateKeyToAccount(secrets.executor).address
    utilityAddress = privateKeyToAccount(secrets.utility).address
  } catch { throw new Error('generated Alto key is outside secp256k1 range') }
  assert.notEqual(executorAddress, utilityAddress, 'canary Alto signer addresses must be distinct')
  const created = []
  try {
    for (const [name, value] of [
      [PARAMETER_NAMES.executor, secrets.executor],
      [PARAMETER_NAMES.utility, secrets.utility],
      [PARAMETER_NAMES.origin, secrets.origin],
    ]) {
      await client.putSecureString(name, value, options.kmsKeyArn)
      created.push(name)
    }
  } catch {
    // The failing write may have reached SSM. A later plan must reconcile it;
    // never retry or delete secrets automatically after an uncertain response.
    throw new Error(`canary secret write stopped; confirmed created: ${created.join(', ') || 'none'}; reconcile all three names in SSM before retrying`)
  }
  return {
    mode: 'applied', account, region: options.region, kmsKeyArn: options.kmsKeyArn,
    parameters: names, executorAddress, utilityAddress,
  }
}

function runAws(profile, region, args, secretStdin) {
  return new Promise((resolve, reject) => {
    const child = spawn('aws', ['--profile', profile, '--region', region, ...args, '--output', 'json', '--no-cli-pager'], {
      stdio: [secretStdin === undefined ? 'ignore' : 'pipe', 'pipe', 'pipe'],
      env: { ...process.env, AWS_CLI_AUTO_PROMPT: 'off', AWS_PAGER: '', AWS_MAX_ATTEMPTS: '2' },
    })
    let stdout = ''
    let stderr = ''
    const timer = setTimeout(() => child.kill('SIGKILL'), 45_000)
    child.stdout.on('data', chunk => { stdout += chunk; if (stdout.length > 1_000_000) child.kill('SIGKILL') })
    child.stderr.on('data', chunk => { stderr += chunk; if (stderr.length > 1_000_000) child.kill('SIGKILL') })
    child.on('error', () => { clearTimeout(timer); reject(new Error('could not start AWS CLI')) })
    child.on('close', code => {
      clearTimeout(timer)
      if (code !== 0) {
        const missing = /\(ParameterNotFound\)/.test(stderr)
        reject(Object.assign(new Error(`AWS ${args[0]} ${args[1]} failed (exit ${code})`), { missing }))
      } else {
        try { resolve(JSON.parse(stdout)) } catch { reject(new Error(`AWS ${args[0]} ${args[1]} returned invalid JSON`)) }
      }
    })
    if (secretStdin !== undefined) {
      child.stdin.on('error', () => {})
      child.stdin.end(secretStdin)
    }
  })
}

export function awsCliClient(region) {
  const call = (args, stdin) => runAws('plether', region, args, stdin)
  return {
    identity: () => call(['sts', 'get-caller-identity']),
    keyMetadata: async arn => (await call(['kms', 'describe-key', '--key-id', arn])).KeyMetadata,
    publicKey: arn => call(['kms', 'get-public-key', '--key-id', arn]),
    parameterExists: async name => {
      try { await call(['ssm', 'get-parameter', '--name', name, '--no-with-decryption', '--query', 'Parameter.Name']); return true }
      catch (error) { if (error.missing) return false; throw error }
    },
    putSecureString: async (name, value, kmsKeyArn) => {
      // AWS CLI expands file:///dev/stdin in --value. Neither argv nor output carries the value.
      await call(['ssm', 'put-parameter', '--name', name, '--type', 'SecureString', '--key-id', kmsKeyArn,
        '--value', 'file:///dev/stdin', '--no-overwrite'], value)
    },
  }
}

async function main() {
  const options = parseOptions(process.argv.slice(2))
  if (options.help) { console.log(usage()); return }
  const result = await execute(options, awsCliClient(options.region))
  console.log(JSON.stringify(result, null, 2))
}

if (process.argv[1] && import.meta.url === new URL(`file://${process.argv[1]}`).href) {
  main().catch(error => { console.error(`Canary credential preflight failed: ${error.message}`); process.exitCode = 1 })
}
