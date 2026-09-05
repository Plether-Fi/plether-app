import assert from 'node:assert/strict'
import { describe, it } from 'node:test'
import {
  CANDLE_INTERVALS,
  DATASET_PRICE_GENERATION_FACTOR,
  validateDeploymentCoverage,
} from '../scripts/validate-candle-volume-coverage.mjs'

const manifest = {
  chainId: 421_614,
  orderRouter: '0x2b9790AD11cE5fB1B91aC3415B08cD1Ec7D0cE0B',
}
const healthyGeneration = DATASET_PRICE_GENERATION_FACTOR + 2 * 5 + 1

function payload(intervalSeconds, overrides = {}) {
  return {
    data: {
      intervalSeconds,
      coverageStart: 0,
      coverageEnd: 172_800,
      coverageComplete: true,
      finalizedThrough: 172_800,
      volumeChainId: manifest.chainId,
      volumeRouter: manifest.orderRouter,
      volumeCoverageStart: 0,
      volumeCoverageEnd: 172_800,
      volumeFinalizedThrough: 172_800,
      volumeCoverageComplete: true,
      datasetGeneration: healthyGeneration,
      ...overrides,
    },
    meta: { blockNumber: 0, cached: false, chainId: manifest.chainId },
  }
}

function response(body, status = 200) {
  return new Response(JSON.stringify(body), {
    status,
    headers: { 'Content-Type': 'application/json' },
  })
}

function intervalFromRequest(input) {
  return Number(new URL(input).searchParams.get('interval'))
}

describe('Sepolia candle volume deployment validator', () => {
  it('requires and validates every canonical current-candle interval', async () => {
    const requested = []
    const results = await validateDeploymentCoverage({
      backendUrl: 'https://backend.example.test',
      manifest,
      retryDelayMs: 0,
      fetchImpl: async (input, init) => {
        const interval = intervalFromRequest(input)
        requested.push(interval)
        assert.equal(init.cache, 'no-store')
        assert.equal(init.headers['Cache-Control'], 'no-cache, no-store, max-age=0')
        return response(payload(interval))
      },
    })

    assert.deepEqual(requested.sort((left, right) => left - right), CANDLE_INTERVALS)
    assert.deepEqual(results.map(({ intervalSeconds }) => intervalSeconds), CANDLE_INTERVALS)
    assert.ok(results.every(({ volumeGeneration, usableVolume }) => (
      volumeGeneration === 5 && usableVolume
    )))
  })

  it('rejects a router that differs from the checked-in manifest', async () => {
    await assert.rejects(validateDeploymentCoverage({
      backendUrl: 'https://backend.example.test',
      manifest,
      intervals: [900],
      retryDelayMs: 0,
      fetchImpl: async () => response(payload(900, {
        volumeRouter: '0x1111111111111111111111111111111111111111',
      })),
    }), /volume router does not match/)
  })

  it('rejects null volume coverage bounds', async () => {
    await assert.rejects(validateDeploymentCoverage({
      backendUrl: 'https://backend.example.test',
      manifest,
      intervals: [300],
      retryDelayMs: 0,
      fetchImpl: async () => response(payload(300, {
        volumeCoverageStart: null,
      })),
    }), /volumeCoverageStart must be a non-negative aligned integer/)
  })

  it('rejects zero volume generation and an unset usable-volume bit', async () => {
    await assert.rejects(validateDeploymentCoverage({
      backendUrl: 'https://backend.example.test',
      manifest,
      intervals: [60],
      retryDelayMs: 0,
      fetchImpl: async () => response(payload(60, {
        datasetGeneration: DATASET_PRICE_GENERATION_FACTOR + 1,
      })),
    }), /volume generation is not positive/)

    await assert.rejects(validateDeploymentCoverage({
      backendUrl: 'https://backend.example.test',
      manifest,
      intervals: [60],
      retryDelayMs: 0,
      fetchImpl: async () => response(payload(60, {
        datasetGeneration: DATASET_PRICE_GENERATION_FACTOR + 2,
      })),
    }), /usable-volume generation bit is not set/)
  })

  it('retries a transient request and succeeds within three attempts', async () => {
    let attempts = 0
    const results = await validateDeploymentCoverage({
      backendUrl: 'https://backend.example.test',
      manifest,
      intervals: [3600],
      retryDelayMs: 0,
      fetchImpl: async () => {
        attempts += 1
        return attempts === 1
          ? response({ error: 'temporarily unavailable' }, 503)
          : response(payload(3600))
      },
    })

    assert.equal(attempts, 2)
    assert.equal(results[0].intervalSeconds, 3600)
  })
})
