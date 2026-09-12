import test from 'node:test'
import assert from 'node:assert/strict'
import { evaluatePreparationSamples } from './aa-preparation-benchmark.mjs'
const fixture = () => [1, 2, 3].flatMap(run => Array.from({ length: 100 }, (_, i) =>
  ({ run, kind: 'warm-new', action: ['deposit', 'open', 'close'][i % 3], outcome: 'success', durationMs: 400 })))
test('requires three complete mixed-action runs', () => {
  assert.equal(evaluatePreparationSamples(fixture()).passed, true)
  assert.equal(evaluatePreparationSamples(fixture().slice(1)).passed, false)
})
test('does not hide failures, timeouts, or cold/retry hits in warm results', () => {
  for (const patch of [{ outcome: 'error' }, { outcome: 'timeout' }, { kind: 'retry' }, { kind: 'cold-new' }]) {
    const samples = fixture(); Object.assign(samples[0], patch)
    assert.equal(evaluatePreparationSamples(samples).passed, false)
  }
})
test('enforces both percentiles in every run', () => {
  const samples = fixture()
  samples.slice(0, 6).forEach(sample => { sample.durationMs = 1100 })
  assert.equal(evaluatePreparationSamples(samples).passed, false)
  assert.equal(evaluatePreparationSamples(fixture().map(sample => ({ ...sample, durationMs: 501 }))).passed, false)
})
