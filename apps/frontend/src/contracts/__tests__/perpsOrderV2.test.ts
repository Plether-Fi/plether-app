import { describe, expect, it } from 'vitest'
import { toEventSelector, toFunctionSelector, type Hex } from 'viem'
import {
  PERPS_ORDER_LIFECYCLE_BOOK_ABI,
  PERPS_ORDER_ROUTER_ABI,
} from '../abis'
import { PERPS_SIDE } from '../perpsConstants'
import {
  deriveAdditionalPerpsMarginForLeverage,
  derivePerpsExecutionBounds,
  executionModeOracleFrozen,
  executionModeMask,
  generatePerpsClientOrderId,
  isPublicPerpsClientOrderId,
  PERPS_CLIENT_ORDER_ID_RESERVED_PREFIX,
  PERPS_EXECUTION_MODE,
  PERPS_EXECUTION_MODE_MASK,
  persistPerpsOrderRequestV2,
  relaxedWebPerpsExecutionBounds,
  restorePerpsOrderRequestV2,
  type PerpsExecutionAssessment,
  type PerpsOrderRequestV2,
} from '../perpsOrderV2'

const ACCOUNT = '0x1111111111111111111111111111111111111111'
const CONFIG_HASH = `0x${'22'.repeat(32)}` as Hex

function assessment(
  overrides: Partial<PerpsExecutionAssessment> = {}
): PerpsExecutionAssessment {
  return {
    mode: PERPS_EXECUTION_MODE.LIVE,
    executionNotionalUsdc: 100n,
    grossAccountDebitUsdc: 20n,
    actionChargeAssessedUsdc: 5n,
    actionChargeCollectedUsdc: 5n,
    explicitFeesUsdc: 4n,
    preSettlementBalanceUsdc: 1_000n,
    postSettlementBalanceUsdc: 980n,
    realizedPnlUsdc: 0n,
    vpiUsdc: 0n,
    carryUsdc: 0n,
    executionFeeUsdc: 4n,
    frozenSpreadUsdc: 0n,
    preTraderClaimUsdc: 0n,
    postTraderClaimUsdc: 0n,
    postPositionSize: 500n,
    postPositionMarginUsdc: 100n,
    postPositionEquityUsdc: 95n,
    postLeverageBps: 52_632n,
    ...overrides,
  }
}

describe('bounded V2 order identity', () => {
  it('rejects zero and the reserved protocol prefix before returning randomness', () => {
    let attempt = 0
    const clientOrderId = generatePerpsClientOrderId((bytes) => {
      bytes.fill(0)
      if (attempt === 1) {
        const prefix = PERPS_CLIENT_ORDER_ID_RESERVED_PREFIX.slice(2)
        for (let index = 0; index < 8; index += 1) {
          bytes[index] = Number.parseInt(prefix.slice(index * 2, index * 2 + 2), 16)
        }
      } else if (attempt === 2) {
        bytes.fill(0x11)
      }
      attempt += 1
      return bytes
    })

    expect(clientOrderId).toBe(`0x${'11'.repeat(32)}`)
    expect(attempt).toBe(3)
    expect(isPublicPerpsClientOrderId(`0x${'0'.repeat(64)}`)).toBe(false)
    expect(isPublicPerpsClientOrderId(
      `${PERPS_CLIENT_ORDER_ID_RESERVED_PREFIX}${'00'.repeat(24)}`
    )).toBe(false)
  })

  it('round-trips every immutable request field without changing bounds', () => {
    const request: PerpsOrderRequestV2 = {
      clientOrderId: `0x${'11'.repeat(32)}`,
      side: PERPS_SIDE.SHORT,
      sizeDelta: 100n,
      marginDelta: 10n,
      targetPrice: 1234n,
      isClose: false,
      bounds: derivePerpsExecutionBounds({
        validUntil: 2_000_000_000n,
        expectedConfigHash: CONFIG_HASH,
        executionBountyUsdc: 3n,
        selectedMaxLeverageBps: 60_000,
        assessments: [assessment()],
      }),
    }

    expect(restorePerpsOrderRequestV2(
      persistPerpsOrderRequestV2(ACCOUNT, request)
    )).toEqual(request)
  })
})

describe('bounded V2 execution protections', () => {
  it('keeps web accounting bounds wide while pinning lifecycle protections', () => {
    const bounds = relaxedWebPerpsExecutionBounds({
      validUntil: 2_000_000_000n,
      expectedConfigHash: CONFIG_HASH,
      executionBountyUsdc: 200_000n,
      executionMode: PERPS_EXECUTION_MODE.LIVE,
    })
    const uint256Max = (1n << 256n) - 1n

    expect(bounds).toEqual({
      validUntil: 2_000_000_000n,
      allowedExecutionModes: PERPS_EXECUTION_MODE_MASK.LIVE,
      expectedConfigHash: CONFIG_HASH,
      maxExecutionBountyUsdc: 200_000n,
      maxExecutionNotionalUsdc: uint256Max,
      maxGrossAccountDebitUsdc: uint256Max,
      maxActionChargeUsdc: uint256Max,
      maxExplicitFeesUsdc: uint256Max,
      maxPostPositionSize: uint256Max,
      minPostSettlementBalanceUsdc: 0n,
      minPostPositionEquityUsdc: 0n,
      maxPostLeverageBps: 0xffff_ffff,
    })
  })

  it('derives the exact extra margin needed to preserve selected leverage', () => {
    const positionSize = 50n * 10n ** 20n
    const prices = [100_000_000n, 100_100_000n]
    const additionalMargin = deriveAdditionalPerpsMarginForLeverage({
      selectedMaxLeverageBps: 50_000,
      marginDelta: 1_000_000_000n,
      prices,
      capPrice: 200_000_000n,
      assessments: [
        assessment({
          postPositionSize: positionSize,
          postPositionEquityUsdc: 999_500_000n,
          postLeverageBps: 50_026n,
        }),
        assessment({
          postPositionSize: positionSize,
          postPositionEquityUsdc: 999_500_000n,
          postLeverageBps: 50_076n,
        }),
      ],
    })

    expect(additionalMargin).toBe(1_500_000n)
  })

  it('adds no leverage margin when every reviewed price already fits', () => {
    expect(deriveAdditionalPerpsMarginForLeverage({
      selectedMaxLeverageBps: 50_000,
      marginDelta: 1_000_000_000n,
      prices: [100_000_000n],
      capPrice: 200_000_000n,
      assessments: [assessment({
        postPositionSize: 50n * 10n ** 20n,
        postPositionEquityUsdc: 1_000_000_000n,
        postLeverageBps: 50_000n,
      })],
    })).toBe(0n)
  })

  it('also replaces margin absorbed by positive execution costs', () => {
    expect(deriveAdditionalPerpsMarginForLeverage({
      selectedMaxLeverageBps: 50_000,
      marginDelta: 10_000_000n,
      prices: [100_000_000n],
      capPrice: 200_000_000n,
      assessments: [assessment({
        actionChargeCollectedUsdc: 15_500_000n,
        carryUsdc: 500_000n,
        postPositionSize: 50n * 10n ** 20n,
        postPositionEquityUsdc: 999_500_000n,
        postLeverageBps: 50_026n,
      })],
    })).toBe(5_500_000n)
  })

  it('pins one regime and derives component-wise exact extrema', () => {
    const bounds = derivePerpsExecutionBounds({
      validUntil: 2_000_000_000n,
      expectedConfigHash: CONFIG_HASH,
      executionBountyUsdc: 30n,
      selectedMaxLeverageBps: 70_000,
      assessments: [
        assessment(),
        assessment({
          executionNotionalUsdc: 120n,
          grossAccountDebitUsdc: 25n,
          explicitFeesUsdc: 6n,
          postSettlementBalanceUsdc: 970n,
          postPositionEquityUsdc: 90n,
          postLeverageBps: 60_000n,
        }),
      ],
    })

    expect(bounds).toMatchObject({
      validUntil: 2_000_000_000n,
      allowedExecutionModes: PERPS_EXECUTION_MODE_MASK.LIVE,
      expectedConfigHash: CONFIG_HASH,
      maxExecutionBountyUsdc: 30n,
      maxExecutionNotionalUsdc: 120n,
      maxGrossAccountDebitUsdc: 30n,
      maxExplicitFeesUsdc: 6n,
      minPostSettlementBalanceUsdc: 970n,
      minPostPositionEquityUsdc: 90n,
      maxPostLeverageBps: 60_000,
    })
    expect(executionModeMask(PERPS_EXECUTION_MODE.FROZEN)).toBe(
      PERPS_EXECUTION_MODE_MASK.FROZEN
    )
  })

  it.each([
    ['Live', false],
    ['FAD', false],
    ['Frozen', true],
    ['Unknown', undefined],
  ] as const)('derives oracle-frozen state from %s execution mode', (mode, expected) => {
    expect(executionModeOracleFrozen(mode)).toBe(expected)
  })

  it('fails review on regime drift or leverage above the selected maximum', () => {
    expect(() => derivePerpsExecutionBounds({
      validUntil: 1n,
      expectedConfigHash: CONFIG_HASH,
      executionBountyUsdc: 1n,
      selectedMaxLeverageBps: 60_000,
      assessments: [
        assessment(),
        assessment({ mode: PERPS_EXECUTION_MODE.FAD }),
      ],
    })).toThrow(/regime changed/)

    expect(() => derivePerpsExecutionBounds({
      validUntil: 1n,
      expectedConfigHash: CONFIG_HASH,
      executionBountyUsdc: 1n,
      selectedMaxLeverageBps: 50_000,
      assessments: [assessment()],
    })).toThrow(/maximum leverage/)
  })
})

describe('V2 ABI snapshots', () => {
  it('pins the nested commit selector and perps LONG/SHORT wire values', () => {
    expect(toFunctionSelector(
      'commitOrder((bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))'
    )).toBe('0xd4da06d2')
    expect(PERPS_SIDE).toEqual({ LONG: 0, SHORT: 1 })

    const commit = PERPS_ORDER_ROUTER_ABI.find((item) =>
      item.type === 'function' && item.name === 'commitOrder'
    )
    expect(commit?.inputs[0]).toMatchObject({
      name: 'request',
      type: 'tuple',
    })
  })

  it('pins lifecycle event topics and fixed enum values', () => {
    expect(toEventSelector(
      'IntentRegistered(uint64,address,bytes32,bytes32,uint256,(bytes32,uint8,uint256,uint256,uint256,bool,(uint64,uint8,bytes32,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint32)))'
    )).toBe('0x0f6c9fee478d0c2a764cb32acbbc94790018467626d72ba895ff43dab1919f4a')

    expect(PERPS_ORDER_LIFECYCLE_BOOK_ABI.some((item) =>
      item.type === 'event' && item.name === 'OrderFinalized'
    )).toBe(true)
    expect(PERPS_EXECUTION_MODE).toEqual({
      NONE: 0,
      LIVE: 1,
      FAD: 2,
      FROZEN: 3,
    })
  })
})
