#!/usr/bin/env node
import assert from 'node:assert/strict'

const origin = process.env.INSIGHTS_API_ORIGIN ?? 'https://insights.plether.com'
const path = '/api/insights/v1/competitions/testnet-trading-2026-09/wallets/0x85a5e2603709bbb7465519eea6d35146d66e6678'
const response = await fetch(new URL(path, origin), { signal: AbortSignal.timeout(30_000), cache: 'no-store' })
assert.equal(response.status, 200)
const body = await response.json()
const data = body.data ?? body
for (const expected of [
  { tx: '0xa583ee512122717defccf27f420f17ac10e1e887777132b67f31d5edbaac8bbd', fee: '1105091433', vpi: '6708298582', actual: -7200207038n, kind: 'ActionChargeSettled', waived: '882780318', recovered: '6930609697', collectedFee: '222311115' },
  { tx: '0x4d3a8cf4d63c62f84b2a1065dc8cb9bc1210da44bc7096a30be77acaeb95f99c', fee: '866453105', vpi: '-1755484993', actual: 5474976305n, kind: 'ActionRebateSettled', waived: '0', recovered: '889031888' },
]) {
  const activity = data.activity?.find(item => item.txHash === expected.tx)
  assert.ok(activity, `Missing activity ${expected.tx}`)
  const e = activity.execution
  assert.ok(e, `Missing execution receipt ${expected.tx}`)
  assert.equal(e.protocolVersion, 'v1.2.3')
  assert.equal(e.status, 'complete', `Settlement evidence pending for ${expected.tx}`)
  assert.equal(e.receipt.executionFeeUsdc, expected.fee)
  assert.equal(e.receipt.vpiUsdc, expected.vpi)
  const r = e.receipt
  assert.equal(BigInt(r.postSettlementBalanceUsdc) - BigInt(r.preSettlementBalanceUsdc)
    + BigInt(r.postTraderClaimBalanceUsdc) - BigInt(r.preTraderClaimBalanceUsdc), expected.actual)
  const settlement = e.settlements.find(item => item.kind === expected.kind)
  assert.ok(settlement)
  assert.equal(settlement.waivedUsdc, expected.waived)
  assert.equal(settlement.recoveredUsdc, expected.recovered)
  if (expected.collectedFee) assert.equal(settlement.protocolFeeCollectedUsdc, expected.collectedFee)
  console.log(`Verified ${expected.tx}: assessed fee, settlement evidence, and actual account change`)
}
