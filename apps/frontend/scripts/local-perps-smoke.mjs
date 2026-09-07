import assert from 'node:assert/strict'
import { setTimeout } from 'node:timers/promises'

// Destructive only to the explicitly local sandbox snapshot. No remote URLs.
const origin = 'http://127.0.0.1:5182'
const stringify = value => JSON.stringify(value, (_, v) => typeof v === 'bigint' ? { $bigint: v.toString() } : v)
async function request(path, body) {
  const response = await fetch(`${origin}/local-market/api/${path}`, body === undefined ? undefined : { method: 'POST', headers: { 'Content-Type': 'application/json', Origin: origin }, body: stringify(body) })
  const result = JSON.parse(await response.text(), (_, v) => v && typeof v === 'object' && '$bigint' in v ? BigInt(v.$bigint) : v)
  assert.equal(response.ok, true, result.error)
  return result
}
async function until(status) {
  for (let i = 0; i < 60; i++) {
    const s = await request('state')
    if (s.protection?.status === status) return s
    if (s.protection?.status === 5) throw new Error(`Protection failed: ${stringify(s.log.slice(0, 4))}`)
    await setTimeout(500)
  }
  throw new Error(`Timed out waiting for protection status ${status}`)
}
async function open(direction = 'long') {
  await request('open', { direction, size: '2000', margin: '500', params: { takeProfitTriggerPrice: direction === 'long' ? 90_000_000n : 110_000_000n, stopLossTriggerPrice: direction === 'long' ? 110_000_000n : 90_000_000n } })
  const s = await until(2)
  assert.equal(s.position.exists, true)
}
await request('reset', {})
await open()
const active = await request('state')
const changed = await request('manage', { action: 'replace', protectionId: active.protection.protectionId, params: { takeProfitTriggerPrice: 95_000_000n, stopLossTriggerPrice: 105_000_000n } })
assert.equal(changed.protection.takeProfitTriggerPrice, 95_000_000n)
assert.equal((await request('manage', { action: 'cancel', protectionId: active.protection.protectionId })).protection.status, 6)
await request('manage', { action: 'create', params: { takeProfitTriggerPrice: 90_000_000n, stopLossTriggerPrice: 110_000_000n } })
await until(2)
console.log('PASS replace, cancel, and create protection on an existing position')
await request('price', { price: '1.11' })
assert.equal((await until(4)).position.exists, false)
console.log('PASS long TP: PendingOpen → Armed → Triggered → Executed')
await request('reset', {})
await open('short')
await request('price', { price: '1.11' })
const stopped = await until(4)
assert.equal(stopped.position.exists, false)
assert.equal(stopped.protection.triggeredLeg, 2)
console.log('PASS short SL: correct inverse-price direction and full close')
await request('reset', {})
await open()
await request('auto', { enabled: false })
await request('price', { price: '1.11' })
await until(3)
await request('expire', {})
assert.equal((await until(8)).position.exists, true)
await request('retry', {})
await until(3)
await request('execute', {})
assert.equal((await until(4)).position.exists, false)
console.log('PASS latched expiry: Triggered → Latched → retry → Executed')
await request('reset', {})
console.log('Reset complete: ready for interactive testing')
