import { readFileSync, writeFileSync } from 'node:fs'
import { createHash } from 'node:crypto'
import assert from 'node:assert/strict'

const pin = JSON.parse(readFileSync(new URL('../../../config/perps/close-preview/arbitrum-sepolia.json', import.meta.url)))
const bytes = readFileSync(new URL('../../../config/perps/close-preview/CfdClosePreview.abi.json', import.meta.url))
assert.equal(createHash('sha256').update(bytes).digest('hex'), pin.contracts.cfdClosePreview.abiSha256)
const abi = JSON.parse(bytes).filter(item => item.type === 'error' || item.name === 'previewClose')
writeFileSync(new URL('../src/contracts/abis/CfdClosePreview.ts', import.meta.url),
  '// Generated from config/perps/close-preview/CfdClosePreview.abi.json.\n' +
  '// Run node scripts/generate-close-preview-abi.mjs from apps/frontend to regenerate.\n' +
  `export const PERPS_CFD_CLOSE_PREVIEW_ABI = ${JSON.stringify(abi, null, 2)} as const\n`)
