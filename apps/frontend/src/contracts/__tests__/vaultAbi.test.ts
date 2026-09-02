import { describe, expect, it } from 'vitest'

import {
  PERPS_HOUSE_POOL_ABI,
  PERPS_PUBLIC_LENS_ABI,
  TRANCHE_VAULT_READ_ABI,
} from '../abis'

describe('tranche vault ABI', () => {
  it('exposes fee-free ERC-4626 share conversion for canonical live pricing', () => {
    const convertToAssets = TRANCHE_VAULT_READ_ABI.find((item) => (
      item.type === 'function' && item.name === 'convertToAssets'
    ))

    expect(convertToAssets).toMatchObject({
      stateMutability: 'view',
      inputs: [{ name: 'shares', type: 'uint256' }],
      outputs: [{ name: 'assets', type: 'uint256' }],
    })
  })

  it('uses the merged asynchronous deposit and redemption lifecycle', () => {
    const functionNames = TRANCHE_VAULT_READ_ABI.flatMap((item) => (
      item.type === 'function' ? [item.name] : []
    ))

    expect(functionNames).toEqual(expect.arrayContaining([
      'getRequestEpochWindow',
      'maxRequestDeposit',
      'maxRequestRedeem',
      'DEPOSIT_COOLDOWN',
      'lastDepositTime',
      'estimateDepositShares',
      'estimateWithdrawShares',
      'estimateRedeemAssets',
      'requestDeposit',
      'requestRedeem',
      'cancelPendingDeposit',
      'cancelRedeemRequest',
      'claimDepositShares',
      'claimRedeem',
      'claimRedeemRefund',
    ]))
    expect(functionNames).not.toEqual(expect.arrayContaining([
      'previewDeposit',
      'previewWithdraw',
      'finalizeDepositEpoch',
    ]))
  })

  it('exposes queue, request, Senior-limit, pause, and terminal-deficit views', () => {
    const lensNames = PERPS_PUBLIC_LENS_ABI.flatMap((item) => (
      item.type === 'function' ? [item.name] : []
    ))
    const poolNames = PERPS_HOUSE_POOL_ABI.flatMap((item) => (
      item.type === 'function' ? [item.name] : []
    ))
    const poolLiquidityView = PERPS_HOUSE_POOL_ABI.find((item) => (
      item.type === 'function' && item.name === 'getPoolLiquidityView'
    ))

    expect(lensNames).toEqual(expect.arrayContaining([
      'getSeniorTranche',
      'getJuniorTranche',
      'getTrancheQueues',
      'getLpRequestState',
    ]))
    expect(poolNames).toEqual(expect.arrayContaining([
      'getPendingTrancheState',
      'maxSeniorExposureUsdc',
      'maxSeniorShareBps',
      'seniorRateBps',
      'getSeniorDepositCapacity',
      'reservedSeniorDepositAssetsUsdc',
      'areSeniorDepositReservationsWithinLimits',
      'minTrancheDepositUsdc',
    ]))
    expect(JSON.stringify(poolLiquidityView)).toContain('currentTerminalDeficitUsdc')
  })
})
