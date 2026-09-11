import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { createSecret, inputsParameter, namespace, parseExecutorKeys } from './frankfurt-aa-secrets.mjs'

const keys = ['1', '2', '3', '4'].map(digit => `0x${digit.repeat(64)}`)
test('Alto secret serialization matches the reviewed comma-separated deployment contract', () => {
  assert.deepEqual(parseExecutorKeys(keys.join(',')), keys)
  for (const value of [JSON.stringify(keys), keys.join(', '), keys.slice(1).join(','),
    [...keys, keys[0]].join(','), Array(4).fill(keys[0]).join(','),
    [`0x${'0'.repeat(64)}`, ...keys.slice(1)].join(',')]) {
    assert.throws(() => parseExecutorKeys(value))
  }
})
test('bootstrap credentials are outside runtime roles and writes reject other namespaces', () => {
  assert(!inputsParameter.startsWith(namespace))
  assert.throws(() => createSecret('/plether/sepolia/keeper-private-key', 'not-a-secret'))
})
test('secure writer does not place JSON payloads in process arguments, overwrite, or persist secrets', () => {
  const helper = readFileSync(new URL('./frankfurt-aa-secrets.mjs', import.meta.url), 'utf8')
  assert.match(helper, /input: input === undefined \? undefined : JSON\.stringify\(input\)/)
  assert.match(helper, /sys\.stdin\.read\(\)/)
  assert(!helper.includes("'--overwrite'"))
  assert(!helper.includes('writeFile'))
})
