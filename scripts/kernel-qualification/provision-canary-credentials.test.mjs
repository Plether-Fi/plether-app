import assert from 'node:assert/strict'
import { createECDH } from 'node:crypto'
import test from 'node:test'
import { createRequire } from 'node:module'
import {
  PARAMETER_NAMES, execute, kmsPublicKeyToAddress, parseOptions,
} from './provision-canary-credentials.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { privateKeyToAccount } = require('viem/accounts')
const account = '123456789012'
const region = 'ap-southeast-1'
const symmetricArn = `arn:aws:kms:${region}:${account}:key/00000000-0000-0000-0000-000000000001`
const sponsorArn = `arn:aws:kms:${region}:${account}:key/00000000-0000-0000-0000-000000000002`
const executor = `0x${'0'.repeat(63)}1`
const utility = `0x${'0'.repeat(63)}2`
const origin = '3'.repeat(64)

function fakeClient(exists = {}) {
  const calls = []
  const client = {
    identity: async () => { calls.push('identity'); return { Account: account, Arn: `arn:aws:iam::${account}:user/test` } },
    keyMetadata: async arn => {
      calls.push('keyMetadata')
      return arn === symmetricArn
        ? { Arn: arn, KeyManager: 'CUSTOMER', KeyState: 'Enabled', KeySpec: 'SYMMETRIC_DEFAULT', KeyUsage: 'ENCRYPT_DECRYPT' }
        : { Arn: arn, KeyManager: 'CUSTOMER', KeyState: 'Enabled', KeySpec: 'ECC_SECG_P256K1', KeyUsage: 'SIGN_VERIFY' }
    },
    parameterExists: async name => { calls.push(`exists:${name}`); return exists[name] === true },
    putSecureString: async (name, value, keyArn) => { calls.push({ name, value, keyArn }) },
  }
  return { client, calls }
}

test('plan reads all three names and never generates or writes a credential', async () => {
  const { client, calls } = fakeClient()
  const result = await execute({ mode: 'plan', region, kmsKeyArn: symmetricArn }, client, () => { throw new Error('generation during plan') })
  assert.deepEqual(result.absentParameters, Object.values(PARAMETER_NAMES))
  assert.equal(result.mode, 'plan')
  assert.equal(calls.filter(value => typeof value === 'object').length, 0)
})

test('existing parameter refuses apply before generating or writing', async () => {
  const { client, calls } = fakeClient({ [PARAMETER_NAMES.utility]: true })
  await assert.rejects(
    execute({ mode: 'apply', region, kmsKeyArn: symmetricArn }, client, () => { throw new Error('generation before refusal') }),
    /already exists: \/plether\/sepolia\/kernel-canary-alto-utility-private-key/,
  )
  assert.equal(calls.filter(value => typeof value === 'object').length, 0)
})

test('apply stores three distinct SecureString inputs but returns only public metadata', async () => {
  const { client, calls } = fakeClient()
  const result = await execute({ mode: 'apply', region, kmsKeyArn: symmetricArn }, client, () => ({ executor, utility, origin }))
  const writes = calls.filter(value => typeof value === 'object')
  assert.deepEqual(writes.map(value => value.name), Object.values(PARAMETER_NAMES))
  assert.deepEqual(writes.map(value => value.value), [executor, utility, origin])
  assert.ok(writes.every(value => value.keyArn === symmetricArn))
  assert.equal(result.executorAddress, privateKeyToAccount(executor).address)
  assert.equal(result.utilityAddress, privateKeyToAccount(utility).address)
  assert.ok(!JSON.stringify(result).includes(executor))
  assert.ok(!JSON.stringify(result).includes(utility))
  assert.ok(!JSON.stringify(result).includes(origin))
})

test('partial write stops without retry or cleanup', async () => {
  const { client } = fakeClient()
  const writes = []
  client.putSecureString = async name => { writes.push(name); if (name === PARAMETER_NAMES.utility) throw new Error('simulated AWS uncertainty') }
  await assert.rejects(
    execute({ mode: 'apply', region, kmsKeyArn: symmetricArn }, client, () => ({ executor, utility, origin })),
    /reconcile all three names in SSM before retrying/,
  )
  assert.deepEqual(writes, [PARAMETER_NAMES.executor, PARAMETER_NAMES.utility])
})

test('confirmation and read-only sponsor mode are exclusive', () => {
  assert.throws(() => parseOptions(['--region', region, '--kms-key-arn', symmetricArn, '--apply']), /confirmation/)
  assert.throws(() => parseOptions(['--region', region, '--sponsor-key-arn', sponsorArn, '--apply']), /read-only/)
  assert.equal(parseOptions(['--region', region, '--kms-key-arn', symmetricArn]).mode, 'plan')
})

test('canonical KMS secp256k1 SPKI derives the sponsor address without a signature', async () => {
  const ecdh = createECDH('secp256k1')
  ecdh.setPrivateKey(Buffer.from(executor.slice(2), 'hex'))
  const prefix = Buffer.from('3056301006072a8648ce3d020106052b8104000a034200', 'hex')
  const der = Buffer.concat([prefix, ecdh.getPublicKey(undefined, 'uncompressed')]).toString('base64')
  assert.equal(kmsPublicKeyToAddress(der), privateKeyToAccount(executor).address)
  const { client } = fakeClient()
  client.publicKey = async () => ({ PublicKey: der, KeySpec: 'ECC_SECG_P256K1', KeyUsage: 'SIGN_VERIFY' })
  const result = await execute({ mode: 'sponsor', region, sponsorKeyArn: sponsorArn }, client)
  assert.equal(result.sponsorSigner, privateKeyToAccount(executor).address)
  assert.throws(() => kmsPublicKeyToAddress(Buffer.concat([Buffer.from('00', 'hex'), Buffer.from(der, 'base64').subarray(1)]).toString('base64')), /noncanonical/)
})
