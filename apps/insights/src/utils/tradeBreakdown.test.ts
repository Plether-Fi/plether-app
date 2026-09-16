import { describe, expect, it } from 'vitest'
import waiver from '../../../../scripts/fixtures/insights-close-waiver.json'
import rebate from '../../../../scripts/fixtures/insights-close-rebate.json'
import type { TradeExecution } from '../api/types'
import { deriveTradeBreakdown, formatTradeUsdc } from './tradeBreakdown'

export const waiverExecution = waiver.execution as TradeExecution
export const rebateExecution = rebate.execution as TradeExecution
const clone = (execution = rebateExecution) => structuredClone(execution)

describe('executed trade reconciliation', () => {
  it('reconciles the partially waived fee from the real close receipt', () => {
    const result = deriveTradeBreakdown(waiverExecution, 'Close')!
    expect(result.fee).toBe(1105091433n)
    expect(result.effectiveFee).toBe(222311115n)
    expect(result.lines).toContainEqual({ label: 'Fee waived', value: 882780318n })
    expect(result.actual).toBe(-7200207038n)
    expect(result.unexplained).toBe(0n)
    expect(result.lines.reduce((sum, row) => sum + row.value, 0n)).toBe(result.actual)
  })
  it('deducts the assessed fee once from the gross rebate', () => {
    const result = deriveTradeBreakdown(rebateExecution, 'Close')!
    expect(result.fee).toBe(866453105n)
    expect(result.vpi).toBe(-1755484993n)
    expect(result.netRebate).toBe(889031888n)
    expect(result.actual).toBe(5474976305n)
    expect(result.notice).toBe('Fee deducted from rebate')
    expect(result.lines.reduce((sum, row) => sum + row.value, 0n)).toBe(result.actual)
  })
  it('does not claim a fee-specific waiver without collection evidence', () => {
    const e = clone(waiverExecution)
    delete e.settlements[0].protocolFeeCollectedUsdc
    expect(deriveTradeBreakdown(e, 'Close')!.lines).toContainEqual({ label: 'Charges waived', value: 882780318n })
    expect(deriveTradeBreakdown(e, 'Close')!.effectiveFee).toBeNull()
  })
  it('keeps receipt amounts available while settlement evidence is pending', () => {
    const e = clone(waiverExecution); e.status = 'pending'
    const result = deriveTradeBreakdown(e, 'Close')!
    expect(result.fee).toBe(1105091433n)
    expect(result.unexplained).toBe(882780318n)
    expect(result.lines.some(line => line.label === 'Fee waived')).toBe(false)
  })
  it('does not label costs withheld from price profit as waived', () => {
    const e = clone(); e.settlements = []
    Object.assign(e.receipt, { realizedPnlUsdc: '100000000', executionFeeUsdc: '1000000', vpiUsdc: '2000000', carryUsdc: '0',
      executionBountyUsdc: '200000', preSettlementBalanceUsdc: '1000000000', postSettlementBalanceUsdc: '1096800000' })
    const result = deriveTradeBreakdown(e, 'Close')!
    expect(result.adjustment).toBe(0n)
    expect(result.notice).toBeNull()
  })
  it.each(['Open', 'open'])('omits realized P&L for an %s/increase and includes its costs', type => {
    const e = clone(); e.settlements = []
    Object.assign(e.receipt, { realizedPnlUsdc: '0', vpiUsdc: '1000000', executionFeeUsdc: '400000', carryUsdc: '10000',
      executionBountyUsdc: '200000', preSettlementBalanceUsdc: '1000000000', postSettlementBalanceUsdc: '998390000' })
    const result = deriveTradeBreakdown(e, type)!
    expect(result.lines.some(line => line.label === 'Directional realized P&L')).toBe(false)
    expect(result.actual).toBe(-1610000n)
    expect(result.unexplained).toBe(0n)
  })
  it('supports partial closes and claim payouts', () => {
    const e = clone()
    const difference = BigInt(e.receipt.postSettlementBalanceUsdc) - BigInt(e.receipt.preSettlementBalanceUsdc)
    e.receipt.postSettlementBalanceUsdc = e.receipt.preSettlementBalanceUsdc
    e.receipt.postTraderClaimBalanceUsdc = difference.toString()
    e.receipt.postPositionSize = '1000'
    const result = deriveTradeBreakdown(e, 'Close')!
    expect(result.actual).toBe(5474976305n)
    expect(result.claimChange).toBe(result.actual)
    expect(result.settlementChange).toBe(0n)
  })
  it('counts a frozen-spread waiver once, preserving gross VPI separately', () => {
    const e = clone(waiverExecution)
    Object.assign(e.receipt, { realizedPnlUsdc: '0', executionFeeUsdc: '1000000', vpiUsdc: '-2000000', carryUsdc: '0',
      frozenSpreadUsdc: '5000000', executionBountyUsdc: '0', preSettlementBalanceUsdc: '10000000', postSettlementBalanceUsdc: '8000000' })
    e.settlements = [
      { kind: 'ActionChargeSettled', assessedUsdc: '4000000', recoveredUsdc: '2000000', waivedUsdc: '2000000' },
      { kind: 'FrozenCloseSpreadSettled', assessedUsdc: '5000000', recoveredUsdc: '1000000', waivedUsdc: '2000000' },
    ]
    const result = deriveTradeBreakdown(e, 'Close')!
    expect(result.actual).toBe(-2000000n)
    expect(result.unexplained).toBe(0n)
    expect(result.lines.reduce((sum, line) => sum + line.value, 0n)).toBe(result.actual)
  })
  it('explains an unpaid rebate with a negative adjustment', () => {
    const e = clone()
    e.settlements[0].recoveredUsdc = '0'; e.settlements[0].waivedUsdc = '889031888'
    e.receipt.postSettlementBalanceUsdc = (BigInt(e.receipt.postSettlementBalanceUsdc) - 889031888n).toString()
    expect(deriveTradeBreakdown(e, 'Close')!.lines).toContainEqual({ label: 'Rebate not paid', value: -889031888n })
    expect(deriveTradeBreakdown(e, 'Close')!.unexplained).toBe(0n)
  })
  it('preserves unknown adjustments without guessing their cause', () => {
    const e = clone(); e.receipt.postSettlementBalanceUsdc = (BigInt(e.receipt.postSettlementBalanceUsdc) + 100n).toString()
    expect(deriveTradeBreakdown(e, 'Close')!.lines).toContainEqual({ label: 'Other settlement adjustment', value: 100n })
  })
  it('rejects missing, malformed, or unsupported evidence', () => {
    expect(deriveTradeBreakdown(null, 'Close')).toBeNull()
    for (const mutate of [
      (e: TradeExecution) => { delete e.receipt.executionFeeUsdc },
      (e: TradeExecution) => { e.receipt.executionFeeUsdc = 'NaN' },
      (e: TradeExecution) => { e.receipt.executionFeeUsdc = '-1' },
      (e: TradeExecution) => { e.protocolVersion = 'unknown' },
      (e: TradeExecution) => { e.settlements.push(e.settlements[0]) },
      (e: TradeExecution) => { e.settlements[0].waivedUsdc = '9000000000' },
    ]) { const e = clone(); mutate(e); expect(deriveTradeBreakdown(e, 'Close')).toBeNull() }
  })
  it('formats exact amounts and never rounds a tiny nonzero cost to zero', () => {
    expect(formatTradeUsdc(-1n, true)).toBe('−<0.01 USDC')
    expect(formatTradeUsdc(0n)).toBe('0.00 USDC')
    expect(formatTradeUsdc(866453105n, false, true)).toBe('866.453105 USDC')
  })
})
