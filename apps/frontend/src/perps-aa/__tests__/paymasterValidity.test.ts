import { concatHex, numberToHex, type Hex } from 'viem'
import { describe, expect, it } from 'vitest'
import {
  PIMLICO_SINGLETON_PAYMASTER_V8,
  pimlicoSponsorshipValidUntil,
} from '../paymasterValidity'

function verifyingPaymasterData(input: {
  validUntil: bigint
  validAfter?: bigint
  combinedModeByte?: number
  signatureBytes?: number
}): Hex {
  return concatHex([
    numberToHex(input.combinedModeByte ?? 1, { size: 1 }),
    numberToHex(input.validUntil, { size: 6 }),
    numberToHex(input.validAfter ?? 0n, { size: 6 }),
    `0x${'11'.repeat(input.signatureBytes ?? 65)}`,
  ])
}

describe('pimlicoSponsorshipValidUntil', () => {
  it('parses the SingletonPaymaster verifying-mode deadline', () => {
    expect(pimlicoSponsorshipValidUntil(
      PIMLICO_SINGLETON_PAYMASTER_V8,
      verifyingPaymasterData({
        validUntil: 1_784_869_349n,
      })
    )).toBe(1_784_869_349n)
  })

  it('fails closed for an unbounded or unknown paymaster format', () => {
    expect(pimlicoSponsorshipValidUntil(
      PIMLICO_SINGLETON_PAYMASTER_V8,
      verifyingPaymasterData({ validUntil: 0n })
    )).toBeUndefined()
    expect(pimlicoSponsorshipValidUntil(
      PIMLICO_SINGLETON_PAYMASTER_V8,
      verifyingPaymasterData({
        validUntil: 1_784_869_349n,
        combinedModeByte: 3,
      })
    )).toBeUndefined()
    expect(pimlicoSponsorshipValidUntil(
      PIMLICO_SINGLETON_PAYMASTER_V8,
      '0x90'
    )).toBeUndefined()
    expect(pimlicoSponsorshipValidUntil(
      '0x1111111111111111111111111111111111111111',
      verifyingPaymasterData({ validUntil: 1_784_869_349n })
    )).toBeUndefined()
  })
})
