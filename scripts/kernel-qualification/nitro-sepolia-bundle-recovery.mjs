import assert from 'node:assert/strict'
import { createRequire } from 'node:module'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { decodeFunctionData, encodeFunctionData, getAddress, isAddressEqual, slice } = require('viem')
const { entryPoint09Abi, getUserOperationHash, toPackedUserOperation } = require('viem/account-abstraction')
const eip7702FactoryMarker = `0x7702${'00'.repeat(18)}`

function supportedInitCode(initCode) {
  // handleOps can contain unrelated EIP-7702 operations. Their initCode
  // marker is not a factory and their hash depends on a delegation address.
  return initCode === '0x' || (initCode.length >= 42 && initCode.slice(0, 42) !== eip7702FactoryMarker)
}

function unpackOperation(packed) {
  const { sender, nonce, initCode, callData, accountGasLimits, preVerificationGas, gasFees, paymasterAndData, signature } = packed
  const operation = {
    sender,
    nonce,
    callData,
    callGasLimit: BigInt(slice(accountGasLimits, 16, 32)),
    verificationGasLimit: BigInt(slice(accountGasLimits, 0, 16)),
    preVerificationGas,
    maxFeePerGas: BigInt(slice(gasFees, 16, 32)),
    maxPriorityFeePerGas: BigInt(slice(gasFees, 0, 16)),
    signature,
  }
  if (initCode !== '0x') {
    operation.factory = getAddress(slice(initCode, 0, 20)).toLowerCase()
    operation.factoryData = initCode.length === 42 ? '0x' : slice(initCode, 20)
  }
  if (paymasterAndData !== '0x') {
    operation.paymaster = getAddress(slice(paymasterAndData, 0, 20)).toLowerCase()
    operation.paymasterVerificationGasLimit = BigInt(slice(paymasterAndData, 20, 36))
    operation.paymasterPostOpGasLimit = BigInt(slice(paymasterAndData, 36, 52))
    operation.paymasterData = slice(paymasterAndData, 52)
  }
  return operation
}

export function recoverOperationFromBundle({ transactions, receipt, entryPoint, chainId, operationHash, paymaster }) {
  assert.equal(transactions.length, 2, 'Require two independent bundle transaction reads')
  for (const transaction of transactions) {
    assert.equal(transaction.hash, receipt.transactionHash, 'Recovered transaction hash differs from receipt')
    assert.equal(transaction.blockHash, receipt.blockHash, 'Recovered transaction block hash differs from receipt')
    assert.equal(transaction.blockNumber, receipt.blockNumber, 'Recovered transaction block differs from receipt')
    assert.ok(transaction.to && isAddressEqual(transaction.to, entryPoint), 'Recovered bundle did not call the pinned EntryPoint')
    assert.equal(transaction.chainId, chainId, 'Recovered transaction chain differs')
  }
  const [{ input }, independent] = transactions
  assert.ok(input.toLowerCase() === independent.input.toLowerCase(), 'Independent RPCs disagree on the included bundle calldata')
  const canonicalInput = input.toLowerCase()
  const decoded = decodeFunctionData({ abi: entryPoint09Abi, data: canonicalInput })
  assert.equal(decoded.functionName, 'handleOps', 'Expected EntryPoint.handleOps calldata')
  assert.ok(encodeFunctionData({ abi: entryPoint09Abi, functionName: 'handleOps', args: decoded.args }).toLowerCase() === canonicalInput,
    'Bundle transaction calldata is not canonical')

  const [packedOperations] = decoded.args
  // The current sponsor uses one exact 157-byte envelope and no separate
  // EntryPoint v0.9 paymaster signature suffix.
  const payerData = paymaster && new RegExp(`^${paymaster.toLowerCase()}[0-9a-f]{378}$`, 'u')
  const matching = packedOperations
    .filter(packed => supportedInitCode(packed.initCode) &&
      (payerData ? payerData.test(packed.paymasterAndData) : packed.paymasterAndData === '0x'))
    .map(packed => ({ packed, operation: unpackOperation(packed) }))
    .filter(({ operation }) => getUserOperationHash({ userOperation: operation, chainId, entryPointAddress: entryPoint,
      entryPointVersion: '0.9' }).toLowerCase() === operationHash)
  assert.equal(matching.length, 1, `Expected exactly one matching ${paymaster ? 'sponsored' : 'ETH'} operation in the canonical bundle`)
  const [{ packed, operation }] = matching
  assert.deepEqual(toPackedUserOperation(operation), packed, 'Recovered operation does not round-trip')
  return operation
}
