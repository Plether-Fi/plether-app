import { promises as fs } from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { diagrams, evidence } from './diagram-catalog.mjs'

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const out = path.join(root, '.gitbook/assets/diagrams')
const check = process.argv.includes('--check')
const W = 640
const M = 32
const INNER = W - M * 2 // 576
const FONT = 22
const LINE = 30

// Published swatches: https://plether.com/media-kit + apps/frontend/src/index.css dark UI tokens.
export const brandPalette = Object.freeze({
  orange: '#FF512F',
  peach: '#FFAB96',
  yellow: '#F7D977',
  paleYellow: '#F7E4B2',
  plum: '#250917',
  panel: '#3B212D',
  gray: '#9A8890',
  mutedInk: '#D8CBD0',
  cream: '#FFF5F9',
  green: '#00FF99',
})
const P = brandPalette
const C = {
  paper: P.plum,
  panel: P.panel,
  ink: P.cream,
  muted: P.mutedInk,
  line: P.gray,
  // High-contrast label colors on dark plum / panel surfaces (all >= 7.4:1 contrast).
  neutral: P.cream,
  accent: P.peach,
  success: P.green,
  warning: P.yellow,
  danger: P.peach,
  // Distinct semantic accent bars / strokes / markers.
  neutralBar: P.gray,
  accentBar: P.peach,
  successBar: P.green,
  warningBar: P.yellow,
  dangerBar: P.orange,
  noteFill: P.panel,
  noteText: P.cream,
}
const markers = {
  neutral: P.gray,
  accent: P.peach,
  success: P.green,
  warning: P.yellow,
  danger: P.orange,
}
const esc = s => String(s).replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;').replaceAll('"', '&quot;')

// Conservative Arial widths; verify-diagrams.mjs measures real Chromium text bounds.
// No foreignObject, web fonts, scripts or external assets in generated SVGs.
function width(text, size) {
  return [...text].reduce((sum, c) => sum + (/[MW@%]/.test(c) ? .92 : /[ilI.,:;'!| ]/.test(c) ? .32 : /[A-Z0-9]/.test(c) ? .70 : .58), 0) * size
}
function wrap(text, maxWidth, size) {
  return String(text).split('\n').flatMap(paragraph => {
    const lines = []; let current = ''
    for (const word of paragraph.split(/\s+/).filter(Boolean)) {
      const candidate = current ? `${current} ${word}` : word
      if (current && width(candidate, size) > maxWidth) { lines.push(current); current = word }
      else current = candidate
    }
    if (current) lines.push(current)
    return lines
  })
}

export function renderDiagram(spec) {
  const parts = []; let y = 0
  function text(value, x, top, maxWidth, size = FONT, color = C.ink, weight = 400, anchor = 'start') {
    const lines = wrap(value, maxWidth, size)
    const leading = size === FONT ? LINE : Math.ceil(size * 1.28)
    for (let i = 0; i < lines.length; i++) {
      const anchorAttr = anchor !== 'start' ? ` text-anchor="${anchor}"` : ''
      parts.push(`<text x="${x}" y="${top + size + i * leading}" font-size="${size}" font-weight="${weight}" fill="${color}" data-max-width="${maxWidth}"${anchorAttr}>${esc(lines[i])}</text>`)
    }
    return lines.length * leading
  }
  const rect = (x, top, w, h, fill, radius = 12, stroke = 'none', strokeWidth = 1.5) =>
    parts.push(`<rect x="${x}" y="${top}" width="${w}" height="${h}" rx="${radius}" fill="${fill}" stroke="${stroke}"${stroke !== 'none' ? ` stroke-width="${strokeWidth}"` : ''}/>`)
  const line = (x1, y1, x2, y2, stroke = C.line, strokeWidth = 1.5, dash = '') =>
    parts.push(`<path d="M${x1} ${y1} L${x2} ${y2}" stroke="${stroke}" stroke-width="${strokeWidth}"${dash ? ` stroke-dasharray="${dash}"` : ''} fill="none"/>`)
  const arrowDown = (x, top, bottom, color = C.line) =>
    parts.push(`<path d="M${x} ${top} V${bottom - 4} m-6 -6 6 6 6 -6" stroke="${color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" fill="none"/>`)
  const arrowRight = (left, right, cy, color = C.line) =>
    parts.push(`<path d="M${left} ${cy} H${right - 4} m-6 -6 6 6 -6 6" stroke="${color}" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round" fill="none"/>`)
  const arrowLeft = (left, right, cy, color = C.line) =>
    parts.push(`<path d="M${right} ${cy} H${left + 4} m6 -6 -6 6 6 6" stroke="${color}" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round" fill="none"/>`)

  function flow(steps, x, top, w, checklist = false) {
    let cursor = top
    steps.forEach((s, i) => {
      const tx = x + 46
      const dotColor = markers[s.tone] ?? P.gray
      parts.push(`<circle cx="${x + 15}" cy="${cursor + 15}" r="13" fill="${P.panel}" stroke="${dotColor}" stroke-width="2.5"/>`)
      parts.push(`<circle cx="${x + 15}" cy="${cursor + 15}" r="5" fill="${dotColor}"/>`)
      let h = text(s.title, tx, cursor, w - 46, 24, C[s.tone] ?? C.ink, 700)
      if (s.body) h += 4 + text(s.body, tx, cursor + h + 4, w - 46, FONT, C.muted)
      if (i < steps.length - 1) {
        if (checklist) line(x + 15, cursor + 34, x + 15, cursor + h + 16, P.gray, 1.5)
        else arrowDown(x + 15, cursor + 34, cursor + h + 20, P.gray)
      }
      cursor += h + 24
    })
    return cursor - top - 8
  }

  // Bespoke visual 1: 3-tier architecture node-and-arrow map (plether-system-overview)
  function renderSystemMap(block) {
    const topH = 114
    rect(M, y, INNER, topH, P.panel, 12, P.peach, 1.5)
    rect(M + 12, y + 3, INNER - 24, 5, P.peach, 3)
    text(block.oracleTitle, M + 20, y + 18, INNER - 40, 24, P.peach, 700)
    text(block.oracleBody, M + 20, y + 52, INNER - 40, FONT, P.mutedInk)
    y += topH

    // Connector from Oracle to Engine
    arrowDown(W / 2, y + 6, y + 40, P.peach)
    y += 44

    // Central Engine Bar
    const engH = 82
    rect(M, y, INNER, engH, P.panel, 12, P.orange, 2)
    text(block.engineTitle, W / 2, y + 14, INNER - 32, 24, P.cream, 700, 'middle')
    text(block.engineSub, W / 2, y + 46, INNER - 32, FONT, P.peach, 400, 'middle')
    y += engH

    // Branch arrows to Traders (left) and LPs (right)
    const leftCx = M + (INNER - 20) / 4
    const rightCx = W - M - (INNER - 20) / 4
    line(W / 2, y + 2, W / 2, y + 18, P.gray, 2)
    line(leftCx, y + 18, rightCx, y + 18, P.gray, 2)
    arrowDown(leftCx, y + 18, y + 42, P.green)
    arrowDown(rightCx, y + 18, y + 42, P.yellow)
    y += 46
  }

  // Bespoke visual 2: Horizontal 0.00 — 1.00 — 2.00 bounded payoff & inverse scale
  function renderPayoffAxis() {
    const boxH = 266
    rect(M, y, INNER, boxH, P.panel, 12, P.gray, 1.5)
    const padX = M + 24
    const trackW = INNER - 48 // 528
    const midX = padX + trackW / 2

    // Row 1: Displayed plDXY label + directional arrows
    text('Displayed plDXY = 2.00 − B', padX, y + 14, trackW, 22, P.cream, 700)

    // Direction cues above track
    text('SHORT USD', padX + 4, y + 50, 200, 22, P.yellow, 700)
    arrowLeft(padX + 140, midX - 18, y + 63, P.yellow)
    arrowRight(midX + 18, padX + trackW - 140, y + 63, P.green)
    text('LONG USD', padX + trackW - 4, y + 50, 200, 22, P.green, 700, 'end')

    // Track bar (2 halves: yellow left, green right)
    const barY = y + 92
    rect(padX, barY, trackW / 2 - 2, 16, P.yellow, 8)
    rect(midX + 2, barY, trackW / 2 - 2, 16, P.green, 8)
    // Center tick
    parts.push(`<circle cx="${midX}" cy="${barY + 8}" r="9" fill="${P.plum}" stroke="${P.cream}" stroke-width="3"/>`)

    // Displayed plDXY scale numbers
    text('0.00', padX, barY + 26, 90, 22, P.cream, 700)
    text('1.00 (Ref)', midX, barY + 26, 150, 22, P.cream, 700, 'middle')
    text('2.00 (Cap)', padX + trackW, barY + 26, 140, 22, P.cream, 700, 'end')

    // Divider
    line(padX, barY + 66, padX + trackW, barY + 66, P.gray, 1, '4 4')

    // Raw basket inverse scale underneath
    text('Raw FX Basket (B)', padX, barY + 78, trackW, 22, P.peach, 700)
    text('2.00', padX, barY + 114, 90, 22, P.mutedInk, 700)
    text('1.00', midX, barY + 114, 90, 22, P.mutedInk, 700, 'middle')
    text('0.00', padX + trackW, barY + 114, 90, 22, P.mutedInk, 700, 'end')

    y += boxH + 22
  }

  // Bespoke visual 3: Horizontal FX weekend schedule strip
  function renderScheduleBar() {
    const boxH = 196
    rect(M, y, INNER, boxH, P.panel, 12, P.gray, 1.5)
    const padX = M + 20
    const stripW = INNER - 40 // 536

    text('Weekly phase strip (not to scale · NY time)', padX, y + 14, stripW, 22, P.mutedInk, 700)

    const barY = y + 54
    const segs = [
      { w: 122, color: P.green, label: 'Open', text: P.plum },
      { w: 72, color: P.yellow, label: '30m', text: P.plum },
      { w: 162, color: P.orange, label: 'Frozen', text: P.plum },
      { w: 72, color: P.yellow, label: '15m', text: P.plum },
      { w: 92, color: P.green, label: 'Open', text: P.plum },
    ]
    let sx = padX
    const gap = 4
    const ticks = []
    segs.forEach((seg, idx) => {
      rect(sx, barY, seg.w, 46, seg.color, 6)
      text(seg.label, sx + seg.w / 2, barY + 10, seg.w - 4, 22, seg.text, 700, 'middle')
      if (idx > 0) ticks.push(sx - gap / 2)
      sx += seg.w + gap
    })

    // Boundary labels staggered cleanly on two rows so zero overlap at 22px
    const [t1, t2, t3, t4] = ticks
    line(t1, barY + 48, t1, barY + 62, P.yellow, 2)
    line(t2, barY + 48, t2, barY + 94, P.orange, 2)
    line(t3, barY + 48, t3, barY + 62, P.orange, 2)
    line(t4, barY + 48, t4, barY + 94, P.green, 2)

    text('Fri 16:30', t1, barY + 64, 130, 22, P.yellow, 700, 'middle')
    text('Fri 17:00', t2, barY + 96, 130, 22, P.peach, 700, 'middle')
    text('Sun 17:00', t3, barY + 64, 130, 22, P.peach, 700, 'middle')
    text('Sun 17:15', t4 - 14, barY + 96, 130, 22, P.green, 700, 'middle')

    y += boxH + 22
  }

  // Bespoke visual 4: Stacked Senior/Junior capital bars (loss vs recovery)
  function renderWaterfallBars() {
    const gap = 20
    const cardW = (INNER - gap) / 2 // 278
    const boxH = 276

    // Left card: Losses eat Junior first, then Senior
    const lx = M
    rect(lx, y, cardW, boxH, P.panel, 12, P.orange, 1.5)
    rect(lx + 12, y + 3, cardW - 24, 5, P.orange, 3)
    text('Loss order', lx + 18, y + 18, cardW - 36, 24, P.peach, 700)
    // Junior block on top (hit 1st)
    rect(lx + 18, y + 60, cardW - 36, 72, P.orange, 8)
    text('1. Junior first', lx + cardW / 2, y + 82, cardW - 48, 22, P.plum, 700, 'middle')
    // Down arrow
    arrowDown(lx + cardW / 2, y + 136, y + 168, P.peach)
    // Senior block below (hit 2nd)
    rect(lx + 18, y + 172, cardW - 36, 82, P.peach, 8)
    text('2. Senior excess', lx + cardW / 2, y + 198, cardW - 48, 22, P.plum, 700, 'middle')

    // Right card: Gains restore Senior HWM first, then Junior
    const rx = M + cardW + gap
    rect(rx, y, cardW, boxH, P.panel, 12, P.green, 1.5)
    rect(rx + 12, y + 3, cardW - 24, 5, P.green, 3)
    text('Recovery order', rx + 18, y + 18, cardW - 36, 24, P.green, 700)
    // Senior restore block first
    rect(rx + 18, y + 60, cardW - 36, 72, P.green, 8)
    text('1. Restore Senior', rx + cardW / 2, y + 82, cardW - 48, 22, P.plum, 700, 'middle')
    // Dashed HWM line (stops cleanly before HWM label)
    line(rx + 18, y + 152, rx + cardW - 82, y + 152, P.yellow, 2, '6 4')
    text('HWM', rx + cardW - 18, y + 137, 62, 22, P.yellow, 700, 'end')
    arrowDown(rx + 56, y + 136, y + 168, P.green)
    // Junior residual block
    rect(rx + 18, y + 172, cardW - 36, 82, P.paleYellow, 8)
    text('2. Residual → Jr', rx + cardW / 2, y + 198, cardW - 48, 22, P.plum, 700, 'middle')

    y += boxH + 22
  }

  // Bespoke visual 5: Before / After stacked horizontal bar for borrow-base carry reduction
  function renderBorrowBar() {
    const boxH = 264
    rect(M, y, INNER, boxH, P.panel, 12, P.gray, 1.5)
    const padX = M + 20
    const barW = INNER - 40 // 536

    text('Same max profit — more margin shrinks carry', padX, y + 14, barW, 22, P.cream, 700)

    // Before row
    text('Before adding margin', padX, y + 54, barW, 22, P.mutedInk, 700)
    const y1 = y + 88
    rect(padX, y1, 136, 44, P.green, 8)
    text('Margin', padX + 68, y1 + 8, 124, 22, P.plum, 700, 'middle')
    rect(padX + 142, y1, barW - 142, 44, P.yellow, 8)
    text('Borrow base (pays carry)', padX + 142 + (barW - 142) / 2, y1 + 8, barW - 154, 22, P.plum, 700, 'middle')

    // After row
    text('After adding position margin', padX, y + 152, barW, 22, P.green, 700)
    const y2 = y + 186
    rect(padX, y2, 316, 44, P.green, 8)
    text('Larger position margin', padX + 158, y2 + 8, 300, 22, P.plum, 700, 'middle')
    rect(padX + 322, y2, barW - 322, 44, P.yellow, 8)
    text('Smaller carry', padX + 322 + (barW - 322) / 2, y2 + 8, barW - 332, 22, P.plum, 700, 'middle')

    y += boxH + 22
  }

  // Bespoke visual 6: 4-phase hourly LP settlement grid (2x2 clean numbered boxes)
  function renderEpochPhases() {
    const gap = 16
    const colW = (INNER - gap) / 2 // 280
    const rowH = 118
    const phases = [
      { n: '1. Senior withdraw', sub: 'Funded first from free cash', color: P.green },
      { n: '2. Junior withdraw', sub: 'After Senior & 80% cap', color: P.yellow },
      { n: '3. Junior deposit', sub: 'Activates at batch NAV', color: P.peach },
      { n: '4. Senior deposit', sub: 'Subject to Senior limits', color: P.peach },
    ]
    text('Strict 4-phase hourly settlement order', M, y + 2, INNER, 24, P.peach, 700)
    y += 38
    phases.forEach((ph, idx) => {
      const col = idx % 2
      const row = Math.floor(idx / 2)
      const px = M + col * (colW + gap)
      const py = y + row * (rowH + gap)
      rect(px, py, colW, rowH, P.panel, 10, ph.color, 1.5)
      rect(px + 10, py + 3, colW - 20, 4, ph.color, 2)
      text(ph.n, px + 16, py + 16, colW - 32, 22, P.cream, 700)
      text(ph.sub, px + 16, py + 48, colW - 32, 22, P.mutedInk, 400)
    })
    y += rowH * 2 + gap + 22
  }

  // Header
  y = M
  y += text(`PLETHER · ${spec.category}`, M, y, INNER, 22, P.peach, 700) + 10
  y += text(spec.title, M, y, INNER, 34, P.cream, 700) + 20
  line(M, y, W - M, y, P.panel, 2)
  parts.push(`<path d="M${M} ${y} h72" stroke="${P.orange}" stroke-width="4" stroke-linecap="round" fill="none"/>`)
  y += 26

  for (const block of spec.blocks) {
    if (block.type === 'systemMap') {
      renderSystemMap(block)
    } else if (block.type === 'payoffAxis') {
      renderPayoffAxis()
    } else if (block.type === 'scheduleBar') {
      renderScheduleBar()
    } else if (block.type === 'waterfallBars') {
      renderWaterfallBars()
    } else if (block.type === 'borrowBar') {
      renderBorrowBar()
    } else if (block.type === 'epochPhases') {
      renderEpochPhases()
    } else if (block.type === 'flow' || block.type === 'checklist') {
      y += flow(block.steps, M, y, INNER, block.type === 'checklist')
    } else if (block.type === 'section') {
      y += text(block.title, M, y + 6, INNER, 24, P.peach, 700) + 20
    } else if (block.type === 'note') {
      const noteTextW = INNER - 44
      const height = wrap(block.text, noteTextW, FONT).length * LINE + 28
      rect(M, y, INNER, height, P.panel, 10, P.gray, 1)
      rect(M + 4, y + 10, 5, height - 20, P.orange, 3)
      text(block.text, M + 24, y + 12, noteTextW, FONT, P.cream)
      y += height + 20
    } else if (block.type === 'lanes') {
      const gap = 20, laneW = (INNER - gap) / 2
      const innerW = laneW - 32
      const heights = block.items.map(item => {
        let height = wrap(item.title, innerW, 24).length * 31 + 38
        item.steps.forEach((s, i) => {
          height += wrap(s.title, innerW, 22).length * 29 + 6
          if (s.body) height += wrap(s.body, innerW, FONT).length * LINE
          height += i < item.steps.length - 1 ? 42 : 18
        })
        return height
      })
      const height = Math.max(...heights)
      block.items.forEach((item, i) => {
        const x = M + i * (laneW + gap)
        const barColor = C[`${item.tone}Bar`] ?? P.gray
        const titleColor = C[item.tone] ?? P.cream
        rect(x, y, laneW, height, P.panel, 12, barColor, 1.5)
        rect(x + 12, y + 3, laneW - 24, 5, barColor, 3)
        let cursor = y + 18
        cursor += text(item.title, x + 16, cursor, innerW, 24, titleColor, 700) + 14
        line(x + 16, cursor - 4, x + laneW - 16, cursor - 4, P.gray, 1)
        item.steps.forEach((s, n) => {
          cursor += text(s.title, x + 16, cursor, innerW, 22, P.cream, 700) + 6
          if (s.body) cursor += text(s.body, x + 16, cursor, innerW, FONT, P.mutedInk)
          if (n < item.steps.length - 1) {
            if (item.alternatives) {
              line(x + 16, cursor + 20, x + laneW / 2 - 26, cursor + 20, P.gray, 1)
              text('OR', x + laneW / 2, cursor + 6, 48, 22, titleColor, 700, 'middle')
              line(x + laneW / 2 + 26, cursor + 20, x + laneW - 16, cursor + 20, P.gray, 1)
            } else {
              arrowDown(x + laneW / 2, cursor + 8, cursor + 32, barColor)
            }
            cursor += 42
          }
        })
      })
      y += height + 22
    } else if (block.type === 'group') {
      y += text(block.title, M, y, INNER, 24, P.peach, 700) + 14
      for (const item of block.items) {
        const h = wrap(item, INNER - 44, FONT).length * LINE + 22
        rect(M, y, INNER, h, P.panel, 8, P.peach, 1)
        rect(M + 4, y + 8, 4, h - 16, P.peach, 2)
        text(item, M + 22, y + 9, INNER - 44, FONT, P.cream)
        y += h + 10
      }
      y += 14
    } else throw new Error(`Unknown block ${block.type}`)
  }

  const height = y + 12
  const description = spec.description + ' ' + spec.blocks.map(b => b.text ?? '').filter(Boolean).join(' ')
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${height}" viewBox="0 0 ${W} ${height}" role="img" aria-labelledby="${spec.id}-title ${spec.id}-desc" focusable="false">
<title id="${spec.id}-title">${esc(spec.title)}</title>
<desc id="${spec.id}-desc">${esc(description)}</desc>
<style>text{font-family:'Uncut Sans',-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,Helvetica,sans-serif}</style>
<rect width="${W}" height="${height}" rx="16" fill="${C.paper}" stroke="${P.panel}" stroke-width="2"/>
${parts.join('\n')}
</svg>\n`
}

async function save(file, content) {
  if (check) {
    if (await fs.readFile(file, 'utf8') !== content) throw new Error(`Stale generated file: ${file}`)
  } else await fs.writeFile(file, content)
}
async function main() {
  const ids = new Set(diagrams.map(d => d.id))
  if (ids.size !== 33 || diagrams.length !== 33) throw new Error('Expected 33 unique diagrams')
  const release = JSON.parse(await fs.readFile(path.resolve(root, '../../config/perps/arbitrum-sepolia-v2.0.0.json'), 'utf8')).release
  const reviewedCommit = '1209ca8a5b94a4fb6323c7c4585846ed93db3932'
  if (release.sourceCommit !== reviewedCommit) throw new Error('Contract release changed: review diagram content and DIAGRAM_REVIEW.md')
  await fs.mkdir(out, { recursive: true })
  const docs = new Map()
  for (const d of diagrams) {
    await save(path.join(out, `${d.id}.svg`), renderDiagram(d))
    const file = path.join(root, d.source)
    let markdown = docs.get(file) ?? await fs.readFile(file, 'utf8')
    const asset = path.relative(path.dirname(file), path.join(out, `${d.id}.svg`)).split(path.sep).join('/')
    const existing = new RegExp(`!\\[[^\\]]*\\]\\([^)]*/${d.id}\\.svg\\)`, 'g')
    if ([...markdown.matchAll(existing)].length !== 1) throw new Error(`Expected one reference for ${d.id} in ${d.source}`)
    markdown = markdown.replace(existing, `![${d.description}](${asset})`)
    docs.set(file, markdown)
  }
  for (const [file, markdown] of docs) await save(file, markdown)
  await save(path.join(out, 'diagram-manifest.json'), JSON.stringify({
    diagramCount: diagrams.length, generator: 'scripts/generate-diagrams.mjs',
    reviewedRelease: { version: release.version, sourceRepository: release.sourceRepository ?? 'Plether-Fi/plether-core', sourceCommit: reviewedCommit },
    design: { width: W, minimumFontSize: 22, articleWidthsChecked: [640, 360, 320], paletteSource: 'https://plether.com/media-kit + apps/frontend/src/index.css', positiveColorSource: 'apps/frontend/src/index.css: --color-positive', palette: brandPalette },
    diagrams: diagrams.map(d => ({ title: d.title, source: d.source, asset: `.gitbook/assets/diagrams/${d.id}.svg`, alt: d.description, evidence: d.evidence.flatMap(key => evidence[key]) })),
  }, null, 2) + '\n')
  console.log(`${check ? 'Verified' : 'Generated'} ${diagrams.length} diagrams and synchronized article references.`)
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) await main()
