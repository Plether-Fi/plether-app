import assert from 'node:assert/strict'
import test from 'node:test'
import { createRequire } from 'node:module'
import { CANARY_SSM_KMS_ARN, CONFIRMATION, PARAMETER_NAME, execute, parseOptions } from './provision-canary-deployer.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { privateKeyToAccount } = require('viem/accounts')
const account = '932542905614'
const region = 'ap-southeast-1'
const kmsKeyArn = CANARY_SSM_KMS_ARN
const privateKey = `0x${'0'.repeat(63)}1`

function fakeClient(exists = false) {
  const writes = []
  const client = {
    identity: async () => ({ Account: account, Arn: `arn:aws:iam::${account}:user/test` }),
    keyMetadata: async () => ({ Arn: kmsKeyArn, KeyManager: 'CUSTOMER', KeyState: 'Enabled',
      KeySpec: 'SYMMETRIC_DEFAULT', KeyUsage: 'ENCRYPT_DECRYPT' }),
    parameterExists: async name => { assert.equal(name, PARAMETER_NAME); return exists },
    putSecureString: async (name, value, arn) => { writes.push({ name, value, arn }) },
  }
  return { client, writes }
}

test('plan checks the exact SSM name without generating or writing', async () => {
  const { client, writes } = fakeClient()
  const result = await execute({ mode: 'plan', region, kmsKeyArn }, client,
    () => { throw Error('plan generated a private key') })
  assert.equal(result.absentParameter, PARAMETER_NAME)
  assert.equal(result.mode, 'plan')
  assert.deepEqual(writes, [])
})

test('an existing parameter refuses apply before generating a replacement', async () => {
  const { client, writes } = fakeClient(true)
  await assert.rejects(execute({ mode: 'apply', region, kmsKeyArn }, client,
    () => { throw Error('generated despite existing parameter') }), /already exists/u)
  assert.deepEqual(writes, [])
})

test('apply writes once through SecureString stdin client and returns no private key', async () => {
  const { client, writes } = fakeClient()
  const result = await execute({ mode: 'apply', region, kmsKeyArn }, client, () => privateKey)
  assert.deepEqual(writes, [{ name: PARAMETER_NAME, value: privateKey, arn: kmsKeyArn }])
  assert.equal(result.deployerAddress, privateKeyToAccount(privateKey).address)
  assert.ok(!JSON.stringify(result).includes(privateKey))
})

test('invalid private key and unexpected AWS key fail before any write', async () => {
  const { client, writes } = fakeClient()
  await assert.rejects(execute({ mode: 'apply', region, kmsKeyArn }, client,
    () => '0x' + '0'.repeat(64)), /outside the secp256k1 range/u)
  client.keyMetadata = async () => ({ Arn: kmsKeyArn, KeyManager: 'CUSTOMER', KeyState: 'Enabled',
    KeySpec: 'ECC_SECG_P256K1', KeyUsage: 'SIGN_VERIFY' })
  await assert.rejects(execute({ mode: 'apply', region, kmsKeyArn }, client,
    () => { throw Error('generated despite wrong KMS key') }), /key metadata changed/u)
  assert.deepEqual(writes, [])
})

test('ambiguous SSM write is never retried or silently cleared', async () => {
  const { client } = fakeClient()
  let calls = 0
  client.putSecureString = async () => { calls++; throw Error('possible network timeout after write') }
  await assert.rejects(execute({ mode: 'apply', region, kmsKeyArn }, client, () => privateKey),
    /may have reached SSM/u)
  assert.equal(calls, 1)
})

test('apply requires exact account, region, key ARN and confirmation', () => {
  assert.equal(parseOptions(['--region', region, '--kms-key-arn', kmsKeyArn]).mode, 'plan')
  assert.equal(parseOptions(['--region', region, '--kms-key-arn', kmsKeyArn,
    '--apply', '--confirm', CONFIRMATION]).mode, 'apply')
  assert.throws(() => parseOptions(['--region', region, '--kms-key-arn', kmsKeyArn, '--apply']), /confirmation/u)
  assert.throws(() => parseOptions(['--region', 'us-east-1', '--kms-key-arn', kmsKeyArn]), /region/u)
  assert.throws(() => parseOptions(['--region', region,
    '--kms-key-arn', 'arn:aws:kms:ap-southeast-1:000000000000:key/00000000-0000-0000-0000-000000000001']), /symmetric KMS/u)
})
