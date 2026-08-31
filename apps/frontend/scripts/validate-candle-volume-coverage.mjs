import { readFile } from 'node:fs/promises'
import { pathToFileURL } from 'node:url'

export const CANDLE_INTERVALS = Object.freeze([60, 180, 300, 900, 1800, 3600, 86400])
export const DATASET_PRICE_GENERATION_FACTOR = 134_217_728
export const DATASET_GENERATION_LIMIT = 67_108_864

function requireAlignedBounds(data, intervalSeconds, prefix) {
  const start = data[`${prefix}Start`]
  const end = data[`${prefix}End`]
  const finalizedThrough = data[prefix === 'coverage'
    ? 'finalizedThrough'
    : 'volumeFinalizedThrough']
  for (const [name, value] of [
    [`${prefix}Start`, start],
    [`${prefix}End`, end],
    [`${prefix}FinalizedThrough`, finalizedThrough],
  ]) {
    if (!Number.isSafeInteger(value) || value < 0 || value % intervalSeconds !== 0) {
      throw new Error(`interval ${intervalSeconds}: ${name} must be a non-negative aligned integer`)
    }
  }
  if (start >= end) {
    throw new Error(`interval ${intervalSeconds}: ${prefix} bounds must contain at least one bucket`)
  }
  if (finalizedThrough < start || finalizedThrough > end) {
    throw new Error(`interval ${intervalSeconds}: ${prefix} finalized bound is outside coverage`)
  }
}

export function decodeDatasetGeneration(datasetGeneration) {
  if (!Number.isSafeInteger(datasetGeneration) || datasetGeneration <= 0) {
    throw new Error('datasetGeneration must be a positive safe integer')
  }
  const priceGeneration = Math.floor(
    datasetGeneration / DATASET_PRICE_GENERATION_FACTOR
  )
  const scopedVolumeGeneration = datasetGeneration % DATASET_PRICE_GENERATION_FACTOR
  return {
    priceGeneration,
    volumeGeneration: Math.floor(scopedVolumeGeneration / 2),
    usableVolume: scopedVolumeGeneration % 2 === 1,
  }
}

export function validateCurrentCandleCoverage(envelope, intervalSeconds, manifest) {
  if (!envelope || typeof envelope !== 'object' || !envelope.data) {
    throw new Error(`interval ${intervalSeconds}: response envelope is missing data`)
  }
  const { data, meta } = envelope
  if (data.intervalSeconds !== intervalSeconds) {
    throw new Error(`interval ${intervalSeconds}: backend returned a mismatched interval`)
  }
  if (data.coverageComplete !== true) {
    throw new Error(`interval ${intervalSeconds}: price coverage is incomplete`)
  }
  requireAlignedBounds(data, intervalSeconds, 'coverage')

  if (data.volumeCoverageComplete !== true) {
    throw new Error(`interval ${intervalSeconds}: volume coverage is incomplete`)
  }
  requireAlignedBounds(data, intervalSeconds, 'volumeCoverage')

  if (data.volumeChainId !== manifest.chainId) {
    throw new Error(`interval ${intervalSeconds}: volume chain does not match the manifest`)
  }
  if (
    typeof data.volumeRouter !== 'string' ||
    data.volumeRouter.toLowerCase() !== manifest.orderRouter.toLowerCase()
  ) {
    throw new Error(`interval ${intervalSeconds}: volume router does not match the manifest`)
  }
  if (meta?.cached !== false) {
    throw new Error(`interval ${intervalSeconds}: direct backend response was unexpectedly cached`)
  }
  if (meta?.chainId !== manifest.chainId) {
    throw new Error(`interval ${intervalSeconds}: response chain does not match the manifest`)
  }

  const generation = decodeDatasetGeneration(data.datasetGeneration)
  if (
    generation.priceGeneration <= 0 ||
    generation.priceGeneration >= DATASET_GENERATION_LIMIT
  ) {
    throw new Error(`interval ${intervalSeconds}: price generation is invalid`)
  }
  if (
    generation.volumeGeneration <= 0 ||
    generation.volumeGeneration >= DATASET_GENERATION_LIMIT
  ) {
    throw new Error(`interval ${intervalSeconds}: volume generation is not positive`)
  }
  if (!generation.usableVolume) {
    throw new Error(`interval ${intervalSeconds}: usable-volume generation bit is not set`)
  }

  return {
    intervalSeconds,
    datasetGeneration: data.datasetGeneration,
    ...generation,
  }
}

function isTransientStatus(status) {
  return status === 408 || status === 425 || status === 429 || status >= 500
}

function delay(milliseconds) {
  return new Promise((resolve) => setTimeout(resolve, milliseconds))
}

async function fetchInterval({
  backendUrl,
  fetchImpl,
  intervalSeconds,
  manifest,
  maxAttempts,
  retryDelayMs,
  timeoutMs,
}) {
  const endpoint = new URL('/api/perps/basket/candles/current', backendUrl)
  endpoint.searchParams.set('interval', intervalSeconds.toString())

  for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
    const controller = new AbortController()
    const timeout = setTimeout(() => controller.abort(), timeoutMs)
    try {
      const response = await fetchImpl(endpoint, {
        cache: 'no-store',
        headers: {
          'Cache-Control': 'no-cache, no-store, max-age=0',
          Pragma: 'no-cache',
        },
        signal: controller.signal,
      })
      if (!response.ok) {
        const error = new Error(
          `interval ${intervalSeconds}: backend returned HTTP ${response.status}`
        )
        error.transient = isTransientStatus(response.status)
        throw error
      }
      const envelope = await response.json()
      return validateCurrentCandleCoverage(envelope, intervalSeconds, manifest)
    } catch (error) {
      const transient = error?.transient === true ||
        error?.name === 'AbortError' ||
        error instanceof TypeError
      if (!transient || attempt === maxAttempts) throw error
      if (retryDelayMs > 0) await delay(retryDelayMs)
    } finally {
      clearTimeout(timeout)
    }
  }
  throw new Error(`interval ${intervalSeconds}: request attempts were exhausted`)
}

export async function validateDeploymentCoverage({
  backendUrl,
  manifest,
  fetchImpl = globalThis.fetch,
  intervals = CANDLE_INTERVALS,
  maxAttempts = 3,
  retryDelayMs = 500,
  timeoutMs = 10_000,
}) {
  if (typeof backendUrl !== 'string' || backendUrl.trim() === '') {
    throw new Error('A backend URL is required')
  }
  const parsedBackendUrl = new URL(backendUrl)
  if (!['http:', 'https:'].includes(parsedBackendUrl.protocol)) {
    throw new Error('The backend URL must use HTTP or HTTPS')
  }
  if (
    !manifest ||
    manifest.chainId !== 421_614 ||
    typeof manifest.orderRouter !== 'string' ||
    !/^0x[0-9a-fA-F]{40}$/.test(manifest.orderRouter)
  ) {
    throw new Error('The checked-in Sepolia manifest has an invalid chain or order router')
  }
  if (typeof fetchImpl !== 'function') throw new Error('A fetch implementation is required')
  if (!Number.isSafeInteger(maxAttempts) || maxAttempts < 1) {
    throw new Error('maxAttempts must be a positive integer')
  }

  return await Promise.all(intervals.map((intervalSeconds) => fetchInterval({
    backendUrl: parsedBackendUrl,
    fetchImpl,
    intervalSeconds,
    manifest,
    maxAttempts,
    retryDelayMs,
    timeoutMs,
  })))
}

function parseArguments(arguments_) {
  const values = new Map()
  for (let index = 0; index < arguments_.length; index += 2) {
    const name = arguments_[index]
    const value = arguments_[index + 1]
    if (!name?.startsWith('--') || value === undefined) {
      throw new Error('Usage: validate-candle-volume-coverage --backend-url URL --manifest PATH')
    }
    values.set(name, value)
  }
  return values
}

async function main() {
  const arguments_ = parseArguments(process.argv.slice(2))
  const backendUrl = arguments_.get('--backend-url')
  const manifestPath = arguments_.get('--manifest')
  if (!backendUrl || !manifestPath) {
    throw new Error('Usage: validate-candle-volume-coverage --backend-url URL --manifest PATH')
  }
  const manifest = JSON.parse(await readFile(manifestPath, 'utf8'))
  const results = await validateDeploymentCoverage({ backendUrl, manifest })
  for (const result of results) {
    process.stdout.write(
      `interval=${result.intervalSeconds} price_generation=${result.priceGeneration} ` +
      `volume_generation=${result.volumeGeneration} usable_volume=true\n`
    )
  }
  process.stdout.write('Sepolia native candle price and volume coverage is deployable.\n')
}

if (process.argv[1] && pathToFileURL(process.argv[1]).href === import.meta.url) {
  main().catch((error) => {
    process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n`)
    process.exitCode = 1
  })
}
