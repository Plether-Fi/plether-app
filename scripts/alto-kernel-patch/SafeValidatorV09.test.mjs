import assert from 'node:assert/strict'
import test from 'node:test'
import { runInNewContext } from 'node:vm'
import { encodeErrorResult, encodeFunctionResult, zeroAddress } from 'viem'
import { EntryPointV07Abi, ERC7769Errors, pimlicoSimulationsAbi } from '../../types/index.js'
import { bundlerCollectorTracer } from './BundlerCollectorTracerV07.js'
import { SafeValidator } from './SafeValidator.js'
import { getTracerBodyString } from './tracer.js'

const entryPoint = '0x433709009B8330FDa32311DF1C2AFA402eD8D009'
const simulation = '0x1000000000000000000000000000000000000001'
const wrapper = '0x1000000000000000000000000000000000000002'
const sender = '0x1000000000000000000000000000000000000003'
const stake = { stake: 0n, unstakeDelaySec: 0n }
const operation = {
  sender,
  nonce: 0n,
  callData: '0x',
  callGasLimit: 100_000n,
  verificationGasLimit: 100_000n,
  preVerificationGas: 100_000n,
  maxFeePerGas: 1n,
  maxPriorityFeePerGas: 1n,
  signature: '0x',
}

function encodedValidation(accountValidationData = 0n) {
  return encodeFunctionResult({
    abi: pimlicoSimulationsAbi,
    functionName: 'simulateValidation',
    result: {
      returnInfo: {
        preOpGas: 42n,
        prefund: 100n,
        accountValidationData,
        paymasterValidationData: 0n,
        paymasterContext: '0x',
      },
      senderInfo: stake,
      factoryInfo: stake,
      paymasterInfo: stake,
      aggregatorInfo: { aggregator: zeroAddress, stakeInfo: stake },
    },
  })
}

function tracedCall(output, success = true, innerOutput = output, error = '') {
  return {
    output,
    error,
    calls: [{
      type: 'REVERT',
      gasUsed: 0,
      data: encodeErrorResult({
        abi: EntryPointV07Abi,
        errorName: 'DelegateAndRevert',
        args: [success, innerOutput],
      }),
    }],
  }
}

async function decode(trace) {
  const validator = Object.create(SafeValidator.prototype)
  validator.logger = { info() {} }
  validator.config = {
    entrypointSimulationContractV9: simulation,
    pimlicoSimulationContract: wrapper,
    publicClient: {
      async request({ method, params }) {
        assert.equal(method, 'debug_traceCall')
        assert.equal(params[0].to.toLowerCase(), wrapper.toLowerCase())
        assert.match(params[2].tracer, /ctx.output/)
        return trace
      },
    },
  }
  return validator.getValidationResultWithTracerV07(operation, [], entryPoint)
}

test('the serialized tracer preserves top-level failure and missing output', () => {
  const tracer = runInNewContext(`(${getTracerBodyString(bundlerCollectorTracer)})`, {
    toHex: output => output,
  })
  const failed = tracer.result({ error: 'out of gas' }, {})
  assert.equal(failed.error, 'out of gas')
  assert.equal(failed.output, '0x')
  const success = tracer.result({ output: '0xabcd' }, {})
  assert.equal(success.error, '')
  assert.equal(success.output, '0xabcd')
})

test('v0.9 decodes the wrapper return after a successful nested revert', async () => {
  const [result] = await decode(tracedCall(encodedValidation()))
  assert.equal(result.returnInfo.preOpGas, 42n)
  assert.equal(result.returnInfo.accountSigFailed, false)
})

test('v0.9 rejects a malformed top-level wrapper return', async () => {
  await assert.rejects(decode(tracedCall('0x1234', true, encodedValidation())), {
    code: ERC7769Errors.SimulateValidation,
    message: 'Malformed EntryPoint simulation result',
  })
})

test('v0.9 rejects an empty, failed or malformed trace frame', async () => {
  for (const trace of [
    null,
    { output: encodedValidation(), calls: [] },
    { output: encodedValidation(), calls: [{ type: 'REVERT', data: '0x1234' }] },
    tracedCall(undefined, true, encodedValidation()),
    tracedCall(encodedValidation(), true, encodedValidation(), 'execution reverted'),
  ]) {
    await assert.rejects(decode(trace), { code: ERC7769Errors.SimulateValidation })
  }
})

test('v0.9 classifies failures from the delegated return bytes', async () => {
  for (const [reason, code, output] of [
    ['AA24 signature error', ERC7769Errors.InvalidSignature, '0x'],
    ['AA34 signature error', ERC7769Errors.InvalidSignature, encodedValidation()],
    ['AA31 paymaster deposit too low', ERC7769Errors.PaymasterDepositTooLow, '0x'],
    ['AA33 revert included unrelated text AA34', ERC7769Errors.SimulateValidation, '0x'],
  ]) {
    const failed = encodeErrorResult({
      abi: EntryPointV07Abi,
      errorName: 'FailedOp',
      args: [0n, reason],
    })
    await assert.rejects(decode(tracedCall(output, false, failed, 'execution reverted')), {
      code,
      message: reason,
    })
  }
})

test('malformed delegated failures stay classified as validation failures', async () => {
  await assert.rejects(decode(tracedCall(encodedValidation(), false, '0x1234')), {
    code: ERC7769Errors.SimulateValidation,
    message: 'Malformed EntryPoint simulation result',
  })
})

test('v0.9 rejects a signature failure reported in ValidationResult', async () => {
  await assert.rejects(decode(tracedCall(encodedValidation(1n))), error => {
    assert.equal(error.code, ERC7769Errors.InvalidSignature)
    return true
  })
})
