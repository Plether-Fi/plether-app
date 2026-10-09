import { decodeAbiParameters, decodeFunctionData, encodeAbiParameters, encodeFunctionData, parseAbi, parseAbiParameters, type Address, type Hex } from 'viem'
import { ACROSS_ARBITRUM_LOGGER } from './sources'
import type { FundingDestination, FundingQuote } from './types'
import { sameAddress } from './validation'

export const FUNDING_INSTRUCTIONS_ABI = parseAbiParameters('((address target,bytes callData,uint256 value)[] calls,address fallbackRecipient)')
export const FUNDING_ACTION_ABI = parseAbi([
  'struct Replacement { address token; uint256 offset; }',
  'function makeCallWithBalance(address target,bytes callData,uint256 value,Replacement[] replacement)',
  'function approve(address spender,uint256 amount)',
  'function depositFor(address account,uint256 amount)',
  'function emitData(bytes data)',
  'function drainLeftoverTokens(address token,address destination)',
])

export function fundingDestinationCalls(destination: FundingDestination, quoteId: Hex) {
  const { token, clearinghouse, beneficiary, multicallHandler } = destination
  const approval = encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'approve', args: [clearinghouse, 0n] })
  const deposit = encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'depositFor', args: [beneficiary, 0n] })
  const withBalance = (target: Address, callData: Hex) => ({ target: multicallHandler, callData: encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'makeCallWithBalance', args: [target, callData, 0n, [{ token, offset: 36n }]] }), value: 0n })
  return [withBalance(token, approval), withBalance(clearinghouse, deposit), { target: token, callData: approval, value: 0n }, { target: ACROSS_ARBITRUM_LOGGER, callData: encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'emitData', args: [quoteId] }), value: 0n }]
}

/** Amount placeholders must be zero: the reviewed handler ORs its current token balance into offset 36. */
export function buildFundingDestinationMessage(destination: FundingDestination, quoteId: Hex): Hex {
  return encodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, [{ calls: fundingDestinationCalls(destination, quoteId), fallbackRecipient: destination.beneficiary }])
}

export function validateFundingDestinationMessage(message: Hex, quote: FundingQuote, destination: FundingDestination): void {
  const reject = () => { throw new Error('The destination actions do not match the reviewed deposit and beneficiary fallback.') }
  if (message.toLowerCase() !== quote.destinationMessage.toLowerCase()) reject()
  const decoded = decodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, message)
  if (encodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, decoded).toLowerCase() !== message.toLowerCase()) reject()
  const { calls, fallbackRecipient } = decoded[0]
  if (!sameAddress(fallbackRecipient, destination.beneficiary) || (calls.length !== 4 && calls.length !== 8)) reject()
  const expected = fundingDestinationCalls(destination, quote.quoteId)
  expected.forEach((call, index) => {
    if (!sameAddress(calls[index].target, call.target) || calls[index].value !== 0n || calls[index].callData.toLowerCase() !== call.callData.toLowerCase()) reject()
  })
  // The previously reviewed provider suffix can only drain remaining USDC to
  // this beneficiary and emit inert metadata. No additional approvals or swaps.
  calls.slice(4).forEach((call, index) => {
    if (call.value !== 0n) reject()
    const action = decodeFunctionData({ abi: FUNDING_ACTION_ABI, data: call.callData })
    if (index < 2) {
      const drain = encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'drainLeftoverTokens', args: [destination.token, destination.beneficiary] })
      if (!sameAddress(call.target, destination.multicallHandler) || call.callData.toLowerCase() !== drain.toLowerCase()) reject()
    } else {
      if (!sameAddress(call.target, ACROSS_ARBITRUM_LOGGER) || action.functionName !== 'emitData' || action.args[0] === '0x') return reject()
      if (encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'emitData', args: action.args }).toLowerCase() !== call.callData.toLowerCase()) reject()
    }
  })
}
