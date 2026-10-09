import { describe, expect, it } from 'vitest'
import { decodeAbiParameters, encodeAbiParameters, encodeFunctionData, zeroAddress, type Hex } from 'viem'
import { FUNDING_ACTION_ABI, FUNDING_INSTRUCTIONS_ABI, buildFundingDestinationMessage, validateFundingDestinationMessage } from './destinationActions'
import fixture from './destinationActions.fixture.json'
import { ACROSS_ARBITRUM_LOGGER } from './sources'
import { destinationFixture, quoteFixture, QUOTE_ID, OTHER_ADDRESS } from './testFixtures'

const destination = destinationFixture()
const message = buildFundingDestinationMessage(destination, QUOTE_ID)
const quote = { ...quoteFixture(), destinationMessage: message }
const decoded = () => decodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, message)[0]

describe('canonical destination deposit actions', () => {
  it('matches the independently viem-generated four-call fixture exactly', () => {
    expect(message.toLowerCase()).toBe(fixture.destinationMessage.toLowerCase())
    expect(() => validateFundingDestinationMessage(message, quote, destination)).not.toThrow()
  })
  it.each([zeroAddress, destination.owner, OTHER_ADDRESS])('rejects a fallback to %s', fallbackRecipient => {
    const bad = encodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, [{ ...decoded(), fallbackRecipient }])
    expect(() => validateFundingDestinationMessage(bad, { ...quote, destinationMessage: bad }, destination)).toThrow()
  })
  it('rejects a marker from another quote even with matching beneficiary and amounts', () => {
    const bad = buildFundingDestinationMessage(destination, `0x${'88'.repeat(32)}`)
    expect(() => validateFundingDestinationMessage(bad, { ...quote, destinationMessage: bad }, destination)).toThrow()
  })
  it.each([0, 1])('rejects nonzero amount placeholders and changed balance replacement offsets in call %i', index => {
    for (const [offset, amount] of [[4n, 0n], [68n, 0n], [36n, 1n]] as const) {
      const instructions = decoded(), calls = [...instructions.calls]
      const inner = encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: index === 0 ? 'approve' : 'depositFor', args: [index === 0 ? destination.clearinghouse : destination.beneficiary, amount] })
      calls[index] = { ...calls[index], callData: encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'makeCallWithBalance', args: [index === 0 ? destination.token : destination.clearinghouse, inner, 0n, [{ token: destination.token, offset }]] }) }
      const bad = encodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, [{ ...instructions, calls }])
      expect(() => validateFundingDestinationMessage(bad, { ...quote, destinationMessage: bad }, destination)).toThrow()
    }
    const instructions = decoded(), calls = [...instructions.calls]
    calls[index] = { ...calls[index], value: 1n }
    const bad = encodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, [{ ...instructions, calls }])
    expect(() => validateFundingDestinationMessage(bad, { ...quote, destinationMessage: bad }, destination)).toThrow()
  })
  it('requires allowance reset and quote marker; an otherwise successful deposit alone is insufficient', () => {
    const instructions = decoded(), calls = instructions.calls.slice(0, 2)
    const bad = encodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, [{ ...instructions, calls }])
    expect(() => validateFundingDestinationMessage(bad, { ...quote, destinationMessage: bad }, destination)).toThrow()
  })
  it('accepts only beneficiary USDC drains and inert logger events in the bounded provider suffix', () => {
    const instructions = decoded()
    const drain = { target: destination.multicallHandler, callData: encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'drainLeftoverTokens', args: [destination.token, destination.beneficiary] }), value: 0n }
    const log = { target: ACROSS_ARBITRUM_LOGGER, callData: encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'emitData', args: ['0x1234'] }), value: 0n }
    const calls = [...instructions.calls, drain, drain, log, log]
    const extended = encodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, [{ ...instructions, calls }])
    expect(() => validateFundingDestinationMessage(extended, { ...quote, destinationMessage: extended }, destination)).not.toThrow()
    calls[4] = { ...drain, callData: encodeFunctionData({ abi: FUNDING_ACTION_ABI, functionName: 'drainLeftoverTokens', args: [destination.token, OTHER_ADDRESS] }) }
    const bad = encodeAbiParameters(FUNDING_INSTRUCTIONS_ABI, [{ ...instructions, calls }])
    expect(() => validateFundingDestinationMessage(bad, { ...quote, destinationMessage: bad }, destination)).toThrow()
  })
  it('rejects unquoted or noncanonical trailing message bytes', () => {
    expect(() => validateFundingDestinationMessage('0x', quote, destination)).toThrow()
    const bad = `${message}00` as Hex
    expect(() => validateFundingDestinationMessage(bad, { ...quote, destinationMessage: bad }, destination)).toThrow()
  })
})
