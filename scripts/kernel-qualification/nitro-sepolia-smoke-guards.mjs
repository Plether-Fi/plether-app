import assert from 'node:assert/strict'

const loopbackHosts = new Set(['localhost', '127.0.0.1', '[::1]', '::1'])
const privateAlbHost = /^internal-[a-z0-9-]+\.ap-southeast-1\.elb\.amazonaws\.com$/u
const legacyNitroGateway = 'https://rpc-internal.sepolia.plether.com/'

export function validateSmokeRpcUrls(values, expected = {}) {
  for (const name of ['alto', 'primary', 'secondary']) {
    const raw = values[name]
    assert.ok(raw, `Set KERNEL_SMOKE_${name.toUpperCase()}_RPC_URL`)
    const url = new URL(raw)
    assert.ok(!url.username && !url.password && !url.search && !url.hash,
      `${name} RPC URL cannot contain credentials, a query or a fragment`)
    if (name === 'secondary') {
      assert.equal(url.protocol, 'https:', 'Independent RPC must use HTTPS')
      continue
    }
    if (name === 'primary' && url.protocol === 'https:') continue
    assert.equal(url.protocol, 'http:', `${name} RPC must use a reviewed localhost tunnel or private AWS ALB`)
    assert.equal(url.pathname, '/', `${name} RPC must use the ALB root path`)
    if (loopbackHosts.has(url.hostname)) continue
    assert.match(url.hostname, privateAlbHost, `${name} RPC must use a private AWS ALB`)
    const label = name === 'primary' ? 'NITRO' : 'ALTO'
    assert.equal(url.hostname, expected[name], `Set the exact reviewed KERNEL_SMOKE_EXPECTED_${label}_ALB_HOST`)
  }
  const primary = new URL(values.primary)
  assert.notEqual(primary.hostname, new URL(values.secondary).hostname, 'Attestation RPCs must have different hosts')
  const legacyAlb = primary.protocol === 'http:' && privateAlbHost.test(primary.hostname)
  if (legacyAlb || primary.href === legacyNitroGateway) {
    assert.ok(expected.bearer, 'Private Nitro requires its bearer token')
  }
  if (expected.bearer) {
    const legacyTunnel = primary.protocol === 'http:' && loopbackHosts.has(primary.hostname)
    assert.ok(legacyAlb || legacyTunnel || primary.href === legacyNitroGateway,
      'The legacy Nitro bearer must only be sent to the private gateway or reviewed tunnel')
    assert.match(expected.bearer, /^[!-~]{32,512}$/u, 'Invalid legacy Nitro bearer format')
  }
  return values
}

export function assertExactGasDebit(before, after, actual, label) {
  assert.ok(actual > 0n, `${label} gas charge must be positive`)
  assert.equal(before - after, actual, `${label} payer debit differs from EntryPoint gas cost`)
}

export async function readSafeBundleReceipts(clients, hash, waitCommonSafe) {
  const receipt = await clients.primary.getTransactionReceipt({ hash })
  await waitCommonSafe(clients, receipt.blockNumber)
  const independent = await clients.secondary.getTransactionReceipt({ hash })
  return [receipt, independent]
}

export function assertRecoveredEthSmokeOperation(operation, expected) {
  assert.equal(operation.sender.toLowerCase(), expected.sender.toLowerCase(), 'Recovered sender differs')
  assert.equal(BigInt(operation.nonce), 0n, 'Recovered smoke operation must use nonce zero')
  assert.equal(operation.callData.toLowerCase(), expected.callData.toLowerCase(), 'Recovered smoke call differs')
  assert.equal(operation.factory?.toLowerCase(), expected.factory.toLowerCase(), 'Recovered KernelFactory differs')
  assert.equal(operation.factoryData?.toLowerCase(), expected.factoryData.toLowerCase(), 'Recovered direct factory deployment data differs')
  for (const field of ['paymaster', 'paymasterData', 'paymasterVerificationGasLimit', 'paymasterPostOpGasLimit',
    'paymasterAndData', 'paymasterSignature', 'eip7702Auth']) {
    assert.equal(operation[field], undefined, `Recovered ETH operation unexpectedly has ${field}`)
  }
  let gas = 0n
  for (const field of ['callGasLimit', 'verificationGasLimit', 'preVerificationGas']) {
    const amount = BigInt(operation[field])
    assert.equal(amount, BigInt(expected.estimate[field]), `Recovered ${field} differs from the reviewed quote`)
    assert.ok(amount > 0n && amount <= expected.gasCap[field], `Recovered ${field} exceeds the smoke cap`)
    gas += amount
  }
  const fee = BigInt(operation.maxFeePerGas)
  const priority = BigInt(operation.maxPriorityFeePerGas)
  assert.ok(fee > 0n && fee <= expected.maxFeeCap && priority <= fee,
    'Recovered fee exceeds the smoke cap')
  assert.equal(gas * fee, BigInt(expected.maxPrefundWei), 'Recovered prefund differs from the reviewed quote')
  assert.ok(gas * fee <= expected.maxPrefundCap, 'Recovered prefund exceeds the 0.05 ETH smoke cap')
}
