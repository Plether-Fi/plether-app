import assert from 'node:assert/strict'
import { createRequire } from 'node:module'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { decodeFunctionData, encodeFunctionData, getAddress, isAddressEqual, slice } = require('viem')
const { entryPoint09Abi, getUserOperationHash, toPackedUserOperation } = require('viem/account-abstraction')

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
    operation.factoryData = slice(initCode, 20)
  }
  if (paymasterAndData !== '0x') {
    operation.paymaster = getAddress(slice(paymasterAndData, 0, 20)).toLowerCase()
    operation.paymasterVerificationGasLimit = BigInt(slice(paymasterAndData, 20, 36))
    operation.paymasterPostOpGasLimit = BigInt(slice(paymasterAndData, 36, 52))
    operation.paymasterData = slice(paymasterAndData, 52)
  }
  assert.deepEqual(toPackedUserOperation(operation), packed, 'Recovered operation does not round-trip')
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
    .filter(packed => payerData ? payerData.test(packed.paymasterAndData) : packed.paymasterAndData === '0x')
    .map(unpackOperation)
    .filter(operation => getUserOperationHash({ userOperation: operation, chainId, entryPointAddress: entryPoint,
      entryPointVersion: '0.9' }).toLowerCase() === operationHash)
  assert.equal(matching.length, 1, `Expected exactly one matching ${paymaster ? 'sponsored' : 'ETH'} operation in the canonical bundle`)
  return matching[0]
}
