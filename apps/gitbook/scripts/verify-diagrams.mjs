// Uses the frontend's already-pinned Playwright; no extra dependency tree.
import { createRequire } from 'node:module'
import { promises as fs } from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { diagrams } from './diagram-catalog.mjs'
import { brandPalette } from './generate-diagrams.mjs'
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const require = createRequire(path.resolve(root, '../frontend/package.json'))
const { chromium } = require('playwright')
const output = process.argv.find(arg => arg.startsWith('--output='))?.slice(9)
if (output) await fs.mkdir(output, { recursive: true })
const browser = await chromium.launch({ headless: true })
const results = []
try {
  const page = await browser.newPage({ viewport: { width: 736, height: 1000 }, deviceScaleFactor: 1 })
  for (const d of diagrams) {
    const svg = await fs.readFile(path.join(root, '.gitbook/assets/diagrams', `${d.id}.svg`), 'utf8')
    if (/<(?:script|foreignObject|image)\b|https?:\/\/(?!www.w3.org\/2000\/svg)/.test(svg)) throw new Error(`Non-portable asset: ${d.id}`)
    for (const w of [640, 360, 320]) {
      await page.setContent(`<style>body{margin:0;background:${brandPalette.plum}}svg{width:${w}px;height:auto;display:block}</style>${svg}`)
      const problems = await page.evaluate(() => {
        const svg = document.querySelector('svg'), box = svg.getBoundingClientRect(), vb = svg.viewBox.baseVal
        const errors = [], texts = [...svg.querySelectorAll('text')]
        const luminance = color => {
          const rgb = color.match(/\d+/g).slice(0, 3).map(n => Number(n) / 255).map(n => n <= .04045 ? n / 12.92 : ((n + .055) / 1.055) ** 2.4)
          return rgb[0] * .2126 + rgb[1] * .7152 + rgb[2] * .0722
        }
        for (const t of texts) {
          const b = t.getBBox(), r = t.getBoundingClientRect(), style = getComputedStyle(t)
          if (b.width > Number(t.dataset.maxWidth) + 1) errors.push(`Text exceeds column: ${t.textContent}`)
          if (r.left < box.left || r.right > box.right + 1 || r.top < box.top || r.bottom > box.bottom + 1) errors.push(`Clipped: ${t.textContent}`)
          if (parseFloat(style.fontSize) * box.width / vb.width < 11) errors.push(`Too small: ${t.textContent}`)
          const surfaces = [...svg.querySelectorAll('rect')].filter(rect => {
            const p = rect.getBBox(); return b.x >= p.x && b.x + b.width <= p.x + p.width && b.y >= p.y && b.y + b.height <= p.y + p.height
          })
          const background = surfaces.at(-1)
          const a = luminance(style.fill), z = luminance(getComputedStyle(background).fill)
          if ((Math.max(a, z) + .05) / (Math.min(a, z) + .05) < 4.5) errors.push(`Low contrast: ${t.textContent}`)
        }
        for (let i = 0; i < texts.length; i++) for (let j = i + 1; j < texts.length; j++) {
          const a = texts[i].getBBox(), b = texts[j].getBBox()
          if (a.x < b.x + b.width - 1 && a.x + a.width > b.x + 1 && a.y < b.y + b.height - 1 && a.y + a.height > b.y + 1) errors.push(`Overlap: ${texts[i].textContent} / ${texts[j].textContent}`)
        }
        if (!svg.querySelector('title')?.textContent || !svg.querySelector('desc')?.textContent) errors.push('Missing accessible description')
        return errors
      })
      if (problems.length) throw new Error(`${d.id} at ${w}px:\n${problems.join('\n')}`)
      if (output && w !== 320) await page.locator('svg').screenshot({ path: path.join(output, `${d.id}-${w}.png`) })
      results.push({ id: d.id, width: w, result: 'pass' })
    }
  }
  if (output) {
    // Contact sheets use the actual SVGs at 360px, without resizing raster files.
    for (let start = 0; start < diagrams.length; start += 6) {
      const items = await Promise.all(diagrams.slice(start, start + 6).map(async d => `<figure><figcaption>${d.id}</figcaption>${await fs.readFile(path.join(root, '.gitbook/assets/diagrams', `${d.id}.svg`), 'utf8')}</figure>`))
      await page.setViewportSize({ width: 1152, height: 1000 })
      await page.setContent(`<style>body{margin:12px;background:${brandPalette.gray};color:${brandPalette.plum};display:grid;grid-template-columns:repeat(3,360px);gap:24px;font:12px Arial}figure{margin:0}figcaption{height:32px}svg{display:block;width:360px;height:auto}</style>${items.join('')}`)
      await page.screenshot({ path: path.join(output, `contact-${start / 6 + 1}.png`), fullPage: true })
    }
    await fs.writeFile(path.join(output, 'verification.json'), JSON.stringify(results, null, 2) + '\n')
  }
  console.log(`PASS: ${diagrams.length} SVGs at 640, 360 and 320px; text bounds, overlaps, contrast and accessibility.`)
} finally { await browser.close() }
