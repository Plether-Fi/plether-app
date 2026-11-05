import test from 'node:test'
import assert from 'node:assert/strict'
import { evaluatePreparationSamples } from './aa-preparation-benchmark.mjs'
const fixture = () => [1, 2, 3].flatMap(run => Array.from({ length: 100 }, (_, i) =>
  ({ run, kind: 'warm-new', action: ['deposit', 'open', 'close'][i % 3], outcome: 'success', durationMs: 400 })))
test('requires three complete runs', () => {
  assert.equal(evaluatePreparationSamples(fixture()).passed, true)
  assert.equal(evaluatePreparationSamples(fixture().slice(1)).passed, false)
})
for (const run of [1, 2, 3]) {
  for (const action of ['deposit', 'open', 'close']) {
    for (const count of [0, 29]) {
      test(`run ${run} rejects ${count} ${action} samples while retaining 100 successful warm samples`, () => {
        const samples = fixture()
        const rows = samples.filter(sample => sample.run === run)
        let retained = 0
        const replacement = action === 'deposit' ? 'open' : 'deposit'
        for (const sample of rows) {
          if (sample.action === action && retained++ >= count) sample.action = replacement
        }
        const result = evaluatePreparationSamples(samples)
        assert.equal(result.passed, false)
        const rejected = result.runs.find(value => value.run === run)
        assert.equal(rejected.count, 100)
        assert.equal(rejected.mix[action], count)
        assert.equal(rejected.errors, 0)
        assert.equal(rejected.timeouts, 0)
        assert.equal(rejected.passed, false)
        assert.ok(result.runs.filter(value => value.run !== run).every(value => value.passed))
      })
    }
  }
}
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
