import { promises as fs } from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { diagrams, evidence } from './diagram-catalog.mjs'

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const out = path.join(root, '.gitbook/assets/diagrams')
const check = process.argv.includes('--check')
const W = 640
const M = 32
const FONT = 24
const LINE = 32
// Published swatches: https://plether.com/media-kit
// Cream is the media-kit page surface (apps/landing/src/styles.css).
// Positive green matches --color-positive in apps/frontend/src/index.css.
export const brandPalette = Object.freeze({
  orange: '#FF512F', peach: '#FFAB96', yellow: '#F7D977', paleYellow: '#F7E4B2',
  plum: '#250917', gray: '#9A8890', cream: '#FFF5F9', green: '#00FF99',
})
const P = brandPalette
const C = {
  paper: P.cream, ink: P.plum, muted: P.plum, line: P.gray,
  // Bright brand colors do not provide enough small-text contrast on cream.
  // Positive green is reserved for markers and accents, never text on cream.
  neutral: P.plum, accent: P.plum, success: P.plum, warning: P.plum, danger: P.plum,
  neutralFill: P.gray, accentFill: P.peach, successFill: P.cream, warningFill: P.paleYellow, dangerFill: P.peach,
  noteFill: P.plum, noteText: P.cream,
}
const markers = { neutral: P.gray, accent: P.orange, success: P.green, warning: P.yellow, danger: P.orange }
const esc = s => String(s).replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;').replaceAll('"', '&quot;')

// Conservative Arial widths; the browser verifier measures actual text bounds.
// No foreignObject, web fonts, scripts or external assets in generated SVGs.
function width(text, size) {
  return [...text].reduce((sum, c) => sum + (/[MW@%]/.test(c) ? .91 : /[ilI.,:;'!| ]/.test(c) ? .3 : /[A-Z0-9]/.test(c) ? .68 : .56), 0) * size
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
  function text(value, x, top, maxWidth, size = FONT, color = C.ink, weight = 400) {
    const lines = wrap(value, maxWidth, size)
    const leading = size === FONT ? LINE : Math.ceil(size * 1.25)
    for (let i = 0; i < lines.length; i++) {
      parts.push(`<text x="${x}" y="${top + size + i * leading}" font-size="${size}" font-weight="${weight}" fill="${color}" data-max-width="${maxWidth}">${esc(lines[i])}</text>`)
    }
    return lines.length * leading
  }
  const rect = (x, top, w, h, fill, radius = 12, stroke = 'none') => parts.push(`<rect x="${x}" y="${top}" width="${w}" height="${h}" rx="${radius}" fill="${fill}" stroke="${stroke}"/>`)
  const line = (x1, y1, x2, y2, stroke = C.line) => parts.push(`<path d="M${x1} ${y1} L${x2} ${y2}" stroke="${stroke}" stroke-width="2" fill="none"/>`)
  const arrow = (x, top, bottom, tone = 'neutral') => parts.push(`<path d="M${x} ${top} V${bottom - 5} m-5 -5 5 5 5 -5" stroke="${C[tone]}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" fill="none"/>`)
  function flow(steps, x, top, w, checklist = false) {
    let cursor = top
    steps.forEach((s, i) => {
      const tx = x + 48
      parts.push(`<circle cx="${x + 15}" cy="${cursor + 17}" r="14" fill="${markers[s.tone]}"/>`)
      parts.push(`<circle cx="${x + 15}" cy="${cursor + 17}" r="5" fill="${C[s.tone]}"/>`)
      let h = text(s.title, tx, cursor, w - 48, 26, C[s.tone], 700)
      if (s.body) h += 6 + text(s.body, tx, cursor + h + 6, w - 48, FONT, C.muted)
      if (i < steps.length - 1) {
        if (checklist) line(x + 15, cursor + 38, x + 15, cursor + h + 17)
        else arrow(x + 15, cursor + 38, cursor + h + 21)
      }
      cursor += h + 28
    })
    return cursor - top - 12
  }
  y = M
  y += text(`PLETHER / ${spec.category}`, M, y, W - M * 2, 22, C.accent, 700) + 14
  y += text(spec.title, M, y, W - M * 2, 36, C.ink, 700) + 24
  line(M, y, W - M, y)
  parts.push(`<path d="M${M} ${y} h64" stroke="${P.orange}" stroke-width="4" fill="none"/>`)
  y += 28
  for (const block of spec.blocks) {
    if (block.type === 'flow' || block.type === 'checklist') {
      y += flow(block.steps, M, y, W - M * 2, block.type === 'checklist')
    } else if (block.type === 'section') {
      y += text(block.title, M, y + 8, W - M * 2, 24, C.accent, 700) + 26
    } else if (block.type === 'note') {
      const height = wrap(block.text, W - M * 2 - 36, FONT).length * LINE + 32
      rect(M, y, W - M * 2, height, C.noteFill)
      text(block.text, M + 18, y + 14, W - M * 2 - 36, FONT, C.noteText)
      y += height + 24
    } else if (block.type === 'lanes') {
      const gap = 20, laneW = (W - M * 2 - gap) / 2
      const heights = block.items.map(item => {
        let height = wrap(item.title, laneW - 32, 26).length * 33 + 36
        item.steps.forEach((s, i) => {
          height += wrap(s.title, laneW - 32, 26).length * 33 + 8
          if (s.body) height += wrap(s.body, laneW - 32, FONT).length * LINE
          height += i < item.steps.length - 1 ? 46 : 20
        })
        return height
      })
      const height = Math.max(...heights)
      block.items.forEach((item, i) => {
        const x = M + i * (laneW + gap)
        rect(x, y, laneW, height, C[`${item.tone}Fill`], 12, item.tone === 'success' ? P.gray : 'none')
        if (item.tone === 'success') rect(x + 12, y + 4, laneW - 24, 6, P.green, 3)
        let cursor = y + 16
        cursor += text(item.title, x + 16, cursor, laneW - 32, 26, C[item.tone], 700) + 18
        line(x + 16, cursor - 4, x + laneW - 16, cursor - 4)
        item.steps.forEach((s, n) => {
          cursor += text(s.title, x + 16, cursor, laneW - 32, 26, C.ink, 700) + 8
          if (s.body) cursor += text(s.body, x + 16, cursor, laneW - 32, FONT, C.muted)
          if (n < item.steps.length - 1) {
            if (item.alternatives) text('OR', x + 16, cursor + 8, laneW - 32, 22, C[item.tone], 700)
            else arrow(x + 24, cursor + 9, cursor + 33, item.tone)
            cursor += 46
          }
        })
      })
      y += height + 24
    } else if (block.type === 'group') {
      y += text(block.title, M, y, W - M * 2, 26, C.accent, 700) + 16
      for (const item of block.items) {
        const h = wrap(item, W - M * 2 - 40, FONT).length * LINE + 24
        rect(M, y, W - M * 2, h, C.accentFill, 8)
        text(item, M + 20, y + 10, W - M * 2 - 40)
        y += h + 10
      }
      y += 16
    } else throw new Error(`Unknown block ${block.type}`)
  }
  const height = y + 8
  const description = spec.description + ' ' + spec.blocks.map(b => b.text ?? '').filter(Boolean).join(' ')
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${height}" viewBox="0 0 ${W} ${height}" role="img" aria-labelledby="${spec.id}-title ${spec.id}-desc" focusable="false">
<title id="${spec.id}-title">${esc(spec.title)}</title>
<desc id="${spec.id}-desc">${esc(description)}</desc>
<style>text{font-family:Arial,Helvetica,sans-serif}</style>
<rect width="${W}" height="${height}" rx="16" fill="${C.paper}"/>
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
  if (ids.size !== 30 || diagrams.length !== 30) throw new Error('Expected 30 unique diagrams')
  const release = JSON.parse(await fs.readFile(path.resolve(root, '../../config/perps/arbitrum-sepolia-v2.json'), 'utf8')).release
  const reviewedCommit = 'd704122c779d4d681d0fa2be517707b7f7df3902'
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
    reviewedRelease: { version: release.version, sourceRepository: release.sourceRepository, sourceCommit: reviewedCommit },
    design: { width: W, minimumFontSize: 22, articleWidthsChecked: [640, 360, 320], paletteSource: 'https://plether.com/media-kit', positiveColorSource: 'apps/frontend/src/index.css: --color-positive', palette: brandPalette },
    diagrams: diagrams.map(d => ({ title: d.title, source: d.source, asset: `.gitbook/assets/diagrams/${d.id}.svg`, alt: d.description, evidence: d.evidence.flatMap(key => evidence[key]) })),
  }, null, 2) + '\n')
  console.log(`${check ? 'Verified' : 'Generated'} ${diagrams.length} diagrams and synchronized article references.`)
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) await main()
