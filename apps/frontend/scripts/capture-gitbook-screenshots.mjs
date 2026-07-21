import { createServer } from 'node:http'
import { promises as fs } from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { chromium } from 'playwright'

const scriptDirectory = path.dirname(fileURLToPath(import.meta.url))
const frontendDirectory = path.resolve(scriptDirectory, '..')
const repositoryDirectory = path.resolve(frontendDirectory, '../..')
const storybookDirectory = path.join(frontendDirectory, 'storybook-static')
const gitbookDirectory = path.join(repositoryDirectory, 'apps/gitbook')
const manifestPath = path.join(gitbookDirectory, 'STORYBOOK_SCREENSHOT_MAP.md')
const outputDirectory = path.join(gitbookDirectory, '.gitbook/assets/screenshots')
const outputIndexPath = path.join(outputDirectory, 'storybook-screenshots.json')

const MIME_TYPES = new Map([
  ['.css', 'text/css; charset=utf-8'],
  ['.gif', 'image/gif'],
  ['.html', 'text/html; charset=utf-8'],
  ['.ico', 'image/x-icon'],
  ['.jpeg', 'image/jpeg'],
  ['.jpg', 'image/jpeg'],
  ['.js', 'text/javascript; charset=utf-8'],
  ['.json', 'application/json; charset=utf-8'],
  ['.mjs', 'text/javascript; charset=utf-8'],
  ['.png', 'image/png'],
  ['.svg', 'image/svg+xml'],
  ['.wasm', 'application/wasm'],
  ['.woff', 'font/woff'],
  ['.woff2', 'font/woff2'],
])

function parseManifest(markdown) {
  const records = []
  const rowPattern = /^\| `([^`]+\.md):(\d+)` \| ([^|]+?) \| .*?\?path=\/story\/([a-z0-9-]+).*? \|/gm

  for (const match of markdown.matchAll(rowPattern)) {
    records.push({
      documentationPath: match[1],
      documentationLine: Number(match[2]),
      altText: match[3].trim(),
      storyId: match[4],
    })
  }

  return records
}

function cleanAltText(value) {
  return value
    .replaceAll('**', '')
    .replaceAll('`', '')
    .replace(/\[([^\]]+)\]\([^)]+\)/g, '$1')
    .replace(/\s+/g, ' ')
    .trim()
}

function safeStoryFilename(storyId) {
  return `storybook-${storyId}.png`
}

function isSupportInstruction(line) {
  return /^\s*\*\s+Screenshot of /i.test(line)
}

function supportInstructionMatches(line, record) {
  if (!isSupportInstruction(line)) return false

  const normalizedLine = line.toLowerCase()
  const normalizedAltText = cleanAltText(record.altText).toLowerCase()

  if (normalizedAltText.includes('open orders')) return normalizedLine.includes('open orders')
  if (normalizedAltText.includes('order history')) return normalizedLine.includes('order history')
  if (normalizedAltText.includes('account or position field')) {
    return normalizedLine.includes('margin account') || normalizedLine.includes('position field')
  }

  return false
}

async function syncManifestLineNumbers(manifest, records) {
  const groupedRecords = new Map()
  for (const record of records) {
    const fileRecords = groupedRecords.get(record.documentationPath) ?? []
    fileRecords.push(record)
    groupedRecords.set(record.documentationPath, fileRecords)
  }

  let updatedManifest = manifest
  let updatedReferences = 0

  for (const [relativeDocumentationPath, fileRecords] of groupedRecords) {
    const documentationPath = path.join(gitbookDirectory, relativeDocumentationPath)
    const lines = (await fs.readFile(documentationPath, 'utf8')).split('\n')
    const usedLineIndexes = new Set()

    for (const record of fileRecords.sort((a, b) => a.documentationLine - b.documentationLine)) {
      const expectedLineIndex = record.documentationLine - 1
      const expectedFilename = safeStoryFilename(record.storyId)
      const candidateLineIndexes = lines
        .map((line, index) => ({ index, line }))
        .filter(({ index, line }) =>
          !usedLineIndexes.has(index) &&
          (line.includes(expectedFilename) || supportInstructionMatches(line, record))
        )
        .sort((a, b) =>
          Math.abs(a.index - expectedLineIndex) - Math.abs(b.index - expectedLineIndex)
        )
        .map(({ index }) => index)
      const lineIndex = candidateLineIndexes[0]

      if (lineIndex === undefined) {
        throw new Error(
          `Could not locate mapped screenshot for ${relativeDocumentationPath}:${record.documentationLine.toString()}`
        )
      }

      usedLineIndexes.add(lineIndex)
      const actualLine = lineIndex + 1
      if (actualLine === record.documentationLine) continue

      const currentReference = `| \`${relativeDocumentationPath}:${record.documentationLine.toString()}\` |`
      const updatedReference = `| \`${relativeDocumentationPath}:${actualLine.toString()}\` |`
      if (!updatedManifest.includes(currentReference)) {
        throw new Error(`Could not update screenshot-map reference ${currentReference}`)
      }

      updatedManifest = updatedManifest.replace(currentReference, updatedReference)
      record.documentationLine = actualLine
      updatedReferences += 1
    }
  }

  if (updatedReferences > 0) {
    await fs.writeFile(manifestPath, updatedManifest)
  }

  return { updatedReferences }
}

async function startStaticServer() {
  const server = createServer(async (request, response) => {
    try {
      const requestUrl = new URL(request.url ?? '/', 'http://127.0.0.1')
      const requestedPath = decodeURIComponent(requestUrl.pathname)
      const relativePath = requestedPath === '/' ? 'index.html' : requestedPath.replace(/^\/+/, '')
      const resolvedPath = path.resolve(storybookDirectory, relativePath)

      if (!resolvedPath.startsWith(`${storybookDirectory}${path.sep}`) && resolvedPath !== storybookDirectory) {
        response.writeHead(403)
        response.end('Forbidden')
        return
      }

      const stat = await fs.stat(resolvedPath)
      const filePath = stat.isDirectory() ? path.join(resolvedPath, 'index.html') : resolvedPath
      const contents = await fs.readFile(filePath)
      const contentType = MIME_TYPES.get(path.extname(filePath).toLowerCase()) ?? 'application/octet-stream'

      response.writeHead(200, {
        'Cache-Control': 'no-store',
        'Content-Type': contentType,
      })
      response.end(contents)
    } catch {
      response.writeHead(404)
      response.end('Not found')
    }
  })

  await new Promise((resolve, reject) => {
    server.once('error', reject)
    server.listen(0, '127.0.0.1', resolve)
  })

  const address = server.address()
  if (!address || typeof address === 'string') throw new Error('Could not determine static server port')

  return {
    baseUrl: `http://127.0.0.1:${address.port.toString()}`,
    close: () => new Promise((resolve, reject) => {
      server.close((error) => {
        if (error) reject(error)
        else resolve()
      })
    }),
  }
}

async function captureStory(page, baseUrl, storyId, outputPath) {
  const browserErrors = []
  const failedRequests = []
  const onPageError = (error) => {
    browserErrors.push(error instanceof Error ? error.message : String(error))
  }
  const onResponse = (response) => {
    if (response.status() >= 400) {
      failedRequests.push(`${response.status().toString()} ${response.url()}`)
    }
  }
  page.on('pageerror', onPageError)
  page.on('response', onResponse)

  const storyUrl = `${baseUrl}/iframe.html?id=${storyId}&viewMode=story`
  try {
    await page.goto(storyUrl, { waitUntil: 'networkidle', timeout: 60_000 })
    await page.waitForFunction(() => {
      const root = document.querySelector('#storybook-root')
      return root && root.children.length > 0
    }, { timeout: 30_000 })
    await page.evaluate(async () => {
      await document.fonts.ready
    })
    await page.addStyleTag({
      content: `
        *,
        *::before,
        *::after {
          animation-delay: 0s !important;
          animation-duration: 0s !important;
          caret-color: transparent !important;
          transition-delay: 0s !important;
          transition-duration: 0s !important;
        }

        [role="tooltip"] {
          display: none !important;
        }
      `,
    })

    await page.mouse.move(1, 1)

    await page.waitForTimeout(storyId.startsWith('perps-trade-ticket--') ? 1_600 : 300)

    const errorDisplay = page.locator('.sb-errordisplay')
    const errorVisible = await errorDisplay.isVisible().catch(() => false)
    if (errorVisible) {
      const diagnostics = [
        ...browserErrors.map((message) => `browser: ${message}`),
        ...failedRequests.map((message) => `request: ${message}`),
      ].join('\n')
      throw new Error(`Story ${storyId} failed to render.${diagnostics ? `\n${diagnostics}` : ''}`)
    }

    const clip = await page.evaluate(() => {
      const visible = (element) => {
        const style = window.getComputedStyle(element)
        const rect = element.getBoundingClientRect()
        return style.display !== 'none' &&
          style.visibility !== 'hidden' &&
          Number(style.opacity) > 0 &&
          rect.width > 1 &&
          rect.height > 1
      }
      const dialogs = [...document.querySelectorAll('[role="dialog"]')].filter(visible)
      const root = document.querySelector('#storybook-root')
      const content = dialogs.length > 0
        ? dialogs
        : root
          ? [
              ...root.querySelectorAll('section, article, table'),
              ...root.querySelectorAll(':scope > * > *'),
            ].filter(visible)
          : []
      const elements = content.length > 0 ? content : root && visible(root) ? [root] : []
      if (elements.length === 0) return undefined

      const rectangles = elements.map((element) => element.getBoundingClientRect())
      const padding = 24
      const documentWidth = Math.max(document.documentElement.scrollWidth, document.body.scrollWidth)
      const documentHeight = Math.max(document.documentElement.scrollHeight, document.body.scrollHeight)
      const left = Math.max(0, Math.min(...rectangles.map((rect) => rect.left + window.scrollX)) - padding)
      const top = Math.max(0, Math.min(...rectangles.map((rect) => rect.top + window.scrollY)) - padding)
      const right = Math.min(
        documentWidth,
        Math.max(...rectangles.map((rect) => rect.right + window.scrollX)) + padding
      )
      const bottom = Math.min(
        documentHeight,
        Math.max(...rectangles.map((rect) => rect.bottom + window.scrollY)) + padding
      )

      return {
        x: Math.floor(left),
        y: Math.floor(top),
        width: Math.max(1, Math.ceil(right - left)),
        height: Math.max(1, Math.ceil(bottom - top)),
      }
    })

    await page.screenshot({
      animations: 'disabled',
      clip,
      fullPage: clip === undefined,
      path: outputPath,
      type: 'png',
    })

    const dimensions = clip === undefined
      ? await page.evaluate(() => ({
          height: Math.max(document.documentElement.scrollHeight, document.body.scrollHeight),
          width: Math.max(document.documentElement.scrollWidth, document.body.scrollWidth),
        }))
      : { height: clip.height, width: clip.width }

    return {
      dimensions,
      storyPath: `/?path=/story/${storyId}`,
    }
  } finally {
    page.off('pageerror', onPageError)
    page.off('response', onResponse)
  }
}

async function syncDocumentation(records) {
  const groupedRecords = new Map()
  for (const record of records) {
    const fileRecords = groupedRecords.get(record.documentationPath) ?? []
    fileRecords.push(record)
    groupedRecords.set(record.documentationPath, fileRecords)
  }

  let replacements = 0
  let retainedSupportInstructions = 0

  for (const [relativeDocumentationPath, fileRecords] of groupedRecords) {
    const documentationPath = path.join(gitbookDirectory, relativeDocumentationPath)
    const lines = (await fs.readFile(documentationPath, 'utf8')).split('\n')
    const usedLineIndexes = new Set()

    for (const record of fileRecords.sort((a, b) => b.documentationLine - a.documentationLine)) {
      const expectedLineIndex = record.documentationLine - 1
      const expectedFilename = safeStoryFilename(record.storyId)
      const candidateLineIndexes = lines
        .map((line, index) => ({ index, line }))
        .filter(({ index, line }) =>
          !usedLineIndexes.has(index) &&
          (line.includes(expectedFilename) || supportInstructionMatches(line, record))
        )
        .sort((a, b) =>
          Math.abs(a.index - expectedLineIndex) - Math.abs(b.index - expectedLineIndex)
        )
        .map(({ index }) => index)
      const lineIndex =
        !usedLineIndexes.has(expectedLineIndex) &&
        (
          /screen\s*shot|screenshot|placeholder/i.test(lines[expectedLineIndex] ?? '') ||
          /\.gitbook\/assets\/screenshots\/storybook-[^)]+\.png/.test(lines[expectedLineIndex] ?? '')
        )
          ? expectedLineIndex
          : candidateLineIndexes[0]
      const sourceLine = lineIndex === undefined ? undefined : lines[lineIndex]
      const isScreenshotReference =
        /screen\s*shot|screenshot|placeholder/i.test(sourceLine ?? '') ||
        /\.gitbook\/assets\/screenshots\/storybook-[^)]+\.png/.test(sourceLine ?? '')
      if (!sourceLine || !isScreenshotReference) {
        throw new Error(`Expected screenshot reference at ${relativeDocumentationPath}:${record.documentationLine.toString()}`)
      }

      usedLineIndexes.add(lineIndex)

      if (isSupportInstruction(sourceLine)) {
        retainedSupportInstructions += 1
        continue
      }

      const assetPath = path.join(outputDirectory, safeStoryFilename(record.storyId))
      const relativeAssetPath = path.relative(path.dirname(documentationPath), assetPath).split(path.sep).join('/')
      lines[lineIndex] = `![${cleanAltText(record.altText)}](${relativeAssetPath})`
      replacements += 1
    }

    await fs.writeFile(documentationPath, lines.join('\n'))
  }

  return { replacements, retainedSupportInstructions }
}

async function main() {
  const manifest = await fs.readFile(manifestPath, 'utf8')
  const records = parseManifest(manifest)
  if (records.length === 0) {
    throw new Error('No screenshot mappings found in the GitBook screenshot map')
  }
  const manifestSyncResult = await syncManifestLineNumbers(manifest, records)
  if (process.argv.includes('--sync-manifest-only')) {
    process.stdout.write(
      `Synchronized ${manifestSyncResult.updatedReferences.toString()} screenshot-map line references.\n`
    )
    return
  }

  const requestedStoryIds = new Set(
    process.argv
      .filter((argument) => argument.startsWith('--story='))
      .map((argument) => argument.slice('--story='.length))
  )
  const availableStoryIds = new Set(records.map((record) => record.storyId))
  const missingStoryIds = [...requestedStoryIds].filter(
    (storyId) => !availableStoryIds.has(storyId)
  )
  if (missingStoryIds.length > 0) {
    throw new Error(`Unknown mapped story IDs: ${missingStoryIds.join(', ')}`)
  }
  const captureRecords = requestedStoryIds.size > 0
    ? records.filter((record) => requestedStoryIds.has(record.storyId))
    : records

  await fs.access(path.join(storybookDirectory, 'index.json'))
  await fs.mkdir(outputDirectory, { recursive: true })

  const uniqueStoryIds = [...new Set(captureRecords.map((record) => record.storyId))].sort()
  const server = await startStaticServer()
  const browser = await chromium.launch({ headless: true })
  const page = await browser.newPage({
    colorScheme: 'dark',
    deviceScaleFactor: 1,
    viewport: { width: 1440, height: 1400 },
  })
  await page.emulateMedia({ reducedMotion: 'reduce' })

  const captures = []

  try {
    for (const [index, storyId] of uniqueStoryIds.entries()) {
      const outputPath = path.join(outputDirectory, safeStoryFilename(storyId))
      process.stdout.write(`[${(index + 1).toString()}/${uniqueStoryIds.length.toString()}] ${storyId}\n`)
      const capture = await captureStory(page, server.baseUrl, storyId, outputPath)
      captures.push({
        asset: path.relative(gitbookDirectory, outputPath).split(path.sep).join('/'),
        storyId,
        ...capture,
      })
    }
  } finally {
    await page.close()
    await browser.close()
    await server.close()
  }

  const syncResult = await syncDocumentation(captureRecords)
  const outputIndex = requestedStoryIds.size > 0
    ? await mergeSelectiveCaptureIndex(
        records,
        captures,
        manifestSyncResult
      )
    : {
        manifest: path.relative(gitbookDirectory, manifestPath),
        mappedReferences: records.length,
        uniqueStories: uniqueStoryIds.length,
        manifestReferencesUpdated: manifestSyncResult.updatedReferences,
        ...syncResult,
        captures,
      }
  await fs.writeFile(outputIndexPath, `${JSON.stringify(outputIndex, null, 2)}\n`)

  process.stdout.write(
    `Generated ${captures.length.toString()} PNGs; replaced ${syncResult.replacements.toString()} placeholders; retained ${syncResult.retainedSupportInstructions.toString()} support instructions.\n`
  )
}

async function mergeSelectiveCaptureIndex(
  records,
  captures,
  manifestSyncResult
) {
  const existingIndex = JSON.parse(await fs.readFile(outputIndexPath, 'utf8'))
  const capturesByStoryId = new Map(
    existingIndex.captures.map((capture) => [capture.storyId, capture])
  )
  for (const capture of captures) capturesByStoryId.set(capture.storyId, capture)

  return {
    ...existingIndex,
    manifest: path.relative(gitbookDirectory, manifestPath),
    mappedReferences: records.length,
    uniqueStories: new Set(records.map((record) => record.storyId)).size,
    manifestReferencesUpdated: manifestSyncResult.updatedReferences,
    replacements:
      records.length - existingIndex.retainedSupportInstructions,
    retainedSupportInstructions: existingIndex.retainedSupportInstructions,
    captures: [...capturesByStoryId.values()].sort((a, b) =>
      a.storyId.localeCompare(b.storyId)
    ),
  }
}

await main()
