import { readFileSync } from 'node:fs'
import { pathToFileURL } from 'node:url'

// Offline evaluator only: never sends operations, signs, funds, or frees budget.
export function evaluatePreparationSamples(samples) {
  if (!Array.isArray(samples)) throw new Error('Expected a sample array')
  for (const sample of samples) {
    if (!sample || ![1, 2, 3].includes(sample.run)
      || !['warm-new', 'cold-new', 'retry'].includes(sample.kind)
      || !['deposit', 'open', 'close'].includes(sample.action)
      || !['success', 'error', 'timeout'].includes(sample.outcome)
      || !Number.isFinite(sample.durationMs) || sample.durationMs < 0) {
      throw new Error('Malformed preparation sample')
    }
  }
  const summarize = rows => {
    const values = rows.map(row => row.durationMs).sort((a, b) => a - b)
    const percentile = p => values.length ? values[Math.ceil(values.length * p) - 1] : null
    return { count: rows.length, errors: rows.filter(row => row.outcome === 'error').length,
      timeouts: rows.filter(row => row.outcome === 'timeout').length,
      p50Ms: percentile(0.5), p95Ms: percentile(0.95) }
  }
  const runs = [1, 2, 3].map(run => {
    const rows = samples.filter(sample => sample.run === run && sample.kind === 'warm-new')
    const summary = summarize(rows)
    const mix = Object.fromEntries(['deposit', 'open', 'close'].map(action => [action, rows.filter(row => row.action === action).length]))
    return { run, ...summary, mix,
      passed: summary.count === 100 && Object.values(mix).every(count => count >= 30)
        && summary.errors === 0 && summary.timeouts === 0 && summary.p50Ms <= 500 && summary.p95Ms <= 1000 }
  })
  return { passed: runs.every(run => run.passed), runs,
    cold: summarize(samples.filter(sample => sample.kind === 'cold-new')),
    retries: summarize(samples.filter(sample => sample.kind === 'retry')) }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const result = evaluatePreparationSamples(JSON.parse(readFileSync(process.argv[2], 'utf8')))
  console.log(JSON.stringify(result, null, 2))
  process.exitCode = result.passed ? 0 : 1
}
