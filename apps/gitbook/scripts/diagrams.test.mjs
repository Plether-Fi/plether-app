import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync, readdirSync } from 'node:fs'
import { diagrams, evidence } from './diagram-catalog.mjs'
import { brandPalette, renderDiagram } from './generate-diagrams.mjs'

const get = id => diagrams.find(d => d.id === id)
const content = id => JSON.stringify(get(id).blocks)

test('every generated asset has exactly one synchronized article reference', () => {
  const assetRoot = new URL('../.gitbook/assets/diagrams/', import.meta.url)
  assert.equal(readdirSync(assetRoot).filter(name => name.endsWith('.svg')).length, 30)
  for (const d of diagrams) {
    const article = readFileSync(new URL(`../${d.source}`, import.meta.url), 'utf8')
    assert.equal(article.split(`/${d.id}.svg)`).length - 1, 1)
    assert.ok(article.includes(`![${d.description}]`))
    assert.equal(readFileSync(new URL(`${d.id}.svg`, assetRoot), 'utf8'), renderDiagram(d))
  }
})

test('all 30 diagrams have unique IDs, source articles and review evidence', () => {
  assert.equal(diagrams.length, 30)
  assert.equal(new Set(diagrams.map(d => d.id)).size, 30)
  for (const d of diagrams) {
    assert.match(d.source, /\.md$/)
    assert.ok(d.description.length > 40)
    assert.ok(d.evidence.length)
    d.evidence.forEach(key => assert.ok(evidence[key]?.length))
    for (const b of d.blocks.filter(b => b.type === 'lanes')) assert.equal(b.items.length, 2)
  }
})
test('manifest evidence matches the catalog without changing the protocol review pin', () => {
  const manifest = JSON.parse(readFileSync(new URL('../.gitbook/assets/diagrams/diagram-manifest.json', import.meta.url), 'utf8'))
  for (const diagram of diagrams) {
    const record = manifest.diagrams.find(item => item.asset.endsWith(`/${diagram.id}.svg`))
    assert.ok(record, diagram.id)
    assert.deepEqual(record.evidence, diagram.evidence.flatMap(key => evidence[key]))
  }
  const review = readFileSync(new URL('../DIAGRAM_REVIEW.md', import.meta.url), 'utf8')
  assert.ok(review.includes(manifest.reviewedRelease.sourceCommit))
})
test('rendering is deterministic, portable and accessible', () => {
  for (const d of diagrams) {
    const svg = renderDiagram(d)
    assert.equal(svg, renderDiagram(d))
    assert.match(svg, /viewBox="0 0 640 \d+"/)
    assert.match(svg, /role="img" aria-labelledby=/)
    assert.match(svg, /<title id=/)
    assert.match(svg, /<desc id=/)
    assert.doesNotMatch(svg, /foreignObject|<script|<image|undefined|NaN/)
  }
})
test('all diagram colors come from the media kit, its cream surface or frontend positive green', () => {
  const landing = readFileSync(new URL('../../landing/src/App.tsx', import.meta.url), 'utf8')
  const styles = readFileSync(new URL('../../landing/src/styles.css', import.meta.url), 'utf8').toUpperCase()
  const swatches = landing.match(/const MEDIA_COLORS = \[([\s\S]*?)\] as const/)[1]
  const frontend = readFileSync(new URL('../../frontend/src/index.css', import.meta.url), 'utf8')
  assert.equal(brandPalette.green, frontend.match(/--color-positive:\s*(#[\da-fA-F]{6})/)[1].toUpperCase())
  for (const [name, color] of Object.entries(brandPalette)) {
    if (name === 'green') continue
    assert.ok(name === 'cream' ? styles.includes(color) : swatches.includes(color), color)
  }
  const allowed = new Set(Object.values(brandPalette))
  const used = new Set()
  for (const d of diagrams) {
    for (const [, color] of renderDiagram(d).matchAll(/(?:fill|stroke)="(#[A-Fa-f0-9]{6})"/g)) {
      assert.ok(allowed.has(color), `${d.id}: off-palette ${color}`)
      used.add(color)
    }
  }
  assert.deepEqual(used, allowed)
})
test('positive green remains an accent, with high-contrast plum text', () => {
  const success = renderDiagram(get('delayed-order-execution-pipeline'))
  assert.ok(success.includes(`fill="${brandPalette.green}"`))
  for (const d of diagrams) {
    const labels = renderDiagram(d).match(/<text\b[^>]*>/g)
    assert.ok(labels.every(label => !label.includes(brandPalette.green)), d.id)
  }
})
test('calendar uses New York DST-aware boundaries and labels its non-scaled sequence', () => {
  const value = content('weekly-market-state-schedule')
  for (const token of ['16:30', '17:00', '17:15', '30 minutes', '15 minutes', 'daylight time', 'standard time', 'not drawn to a time scale']) assert.ok(value.includes(token), token)
})
test('order failure is not a transition after successful execution', () => {
  const terminal = get('sponsorship-vs-order-failure-lifecycles').blocks.find(b => b.type === 'lanes').items[1]
  assert.equal(terminal.alternatives, true)
  assert.deepEqual(terminal.steps.map(s => s.title), ['Executed', 'Failed'])
  assert.match(content('confirmed-order-execution-path'), /Execution does not succeed/)
  assert.match(content('sponsorship-vs-order-failure-lifecycles'), /panic is not automatically a terminal/)
})
test('claim payout branches and isolated price/action funding are retained', () => {
  for (const id of ['claim-to-owner-wallet', 'trader-claim-lifecycle', 'claim-settlement-funding-path']) {
    assert.match(content(id), /Position still open/)
    assert.match(content(id), /Credit PnL pledge/)
    assert.match(content(id), /No open position/)
  }
  assert.match(content('final-collection-priority'), /Net own claims/)
  assert.match(content('final-collection-priority'), /partial close cannot waive/)
  assert.match(content('pnl-and-close-settlement-outcomes'), /unpaid rebate is waived/)
})
test('sponsorship data is prepared before the owner signature', () => {
  const flow = get('authorization-and-gas-sponsorship').blocks[0].steps
  assert.ok(flow.findIndex(s => s.title === 'Prepare sponsorship') < flow.findIndex(s => s.title === 'Sign in your wallet'))
})
