import { pathToFileURL } from 'node:url'
import { createPublicClient, createWalletClient, formatEther, http, parseAbi } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'

const ARBITRUM_SEPOLIA_CHAIN_ID = 421614

const ADDRESSES = {
  orderRouter: '0x6215d36fcbd610ca1525252eebcbfd8b223a6072',
  pletherOracle: '0x9f4d9ae736b94249b18a85a7e14092bfca0688eb',
  perpsPublicLens: '0x63a6ee8ef44cf13d0d1f393a1e9f8d25da1abfb4',
}

const PERPS_PUBLIC_LENS_ABI = [
  {
    type: 'function',
    name: 'getProtocolStatus',
    stateMutability: 'view',
    inputs: [],
    outputs: [
      {
        name: 'viewData',
        type: 'tuple',
        components: [
          { name: 'phase', type: 'uint8' },
          { name: 'lastMarkPrice', type: 'uint256' },
          { name: 'lastMarkTime', type: 'uint64' },
          { name: 'oracleFrozen', type: 'bool' },
          { name: 'fadWindow', type: 'bool' },
          { name: 'tradingActive', type: 'bool' },
          { name: 'withdrawalLive', type: 'bool' },
          { name: 'lpEpochSettlementPaused', type: 'bool' },
        ],
      },
    ],
  },
]

const PLETHER_ORACLE_ABI = [
  ...parseAbi(['function pyth() view returns (address)', 'function pythFeedIds(uint256) view returns (bytes32)']),
  {
    type: 'function',
    name: 'getUpdateFee',
    stateMutability: 'view',
    inputs: [{ name: 'pythUpdateData', type: 'bytes[]' }],
    outputs: [{ name: 'pythFee', type: 'uint256' }],
  },
]

const ORDER_ROUTER_ABI = [
  {
    type: 'function',
    name: 'updateMarkPrice',
    stateMutability: 'payable',
    inputs: [{ name: 'pythUpdateData', type: 'bytes[]' }],
    outputs: [],
  },
]

const PYTH_ABI = parseAbi([
  'function getPriceUnsafe(bytes32 id) view returns ((int64 price,uint64 conf,int32 expo,uint256 publishTime))',
])

export async function loadOracleFeeds(publicClient) {
  const block = await publicClient.getBlock({ blockTag: 'latest' })
  const read = (functionName, args) => publicClient.readContract({
    address: ADDRESSES.pletherOracle, abi: PLETHER_ORACLE_ABI, functionName, args, blockNumber: block.number,
  })
  const [pyth, ...feedIds] = await Promise.all([
    read('pyth'), ...Array.from({ length: 6 }, (_, index) => read('pythFeedIds', [BigInt(index)])),
  ])
  if (!/^0x[0-9a-f]{40}$/i.test(pyth) || /^0x0{40}$/i.test(pyth) ||
      new Set(feedIds).size !== 6 || feedIds.some(id => !/^0x[0-9a-f]{64}$/i.test(id))) {
    throw new Error('Oracle did not return a valid Pyth address and six distinct feeds')
  }
  return { pyth, feedIds }
}

async function readHealth(publicClient, feeds) {
  const block = await publicClient.getBlock({ blockTag: 'latest' })
  const [status, ...prices] = await Promise.all([
    publicClient.readContract({ address: ADDRESSES.perpsPublicLens, abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'getProtocolStatus', blockNumber: block.number }),
    ...feeds.feedIds.map(id => publicClient.readContract({ address: feeds.pyth, abi: PYTH_ABI,
      functionName: 'getPriceUnsafe', args: [id], blockNumber: block.number })),
  ])
  const oldest = prices.reduce((min, price) => price.publishTime < min ? price.publishTime : min, prices[0].publishTime)
  return { status, block, oldest, lag: status.lastMarkTime > oldest ? status.lastMarkTime - oldest : 0n }
}

function requiredEnv(...names) {
  for (const name of names) {
    const value = process.env[name]
    if (value) return value
  }
  throw new Error(`${names.join(' or ')} is required`)
}

function rpcTransport(rpcUrl) {
  const bearerToken = process.env.PERPS_RPC_AUTH_TOKEN
  return http(rpcUrl, bearerToken
    ? { fetchOptions: { headers: { Authorization: `Bearer ${bearerToken}` } } }
    : undefined)
}

function optionalPrivateKey() {
  const value = process.env.PERPS_ORACLE_UPDATER_PRIVATE_KEY
  if (!value) return undefined
  return value.startsWith('0x') ? value : `0x${value}`
}

function readFlag(name, fallback) {
  const index = process.argv.indexOf(name)
  if (index === -1 || index + 1 >= process.argv.length) return fallback
  return process.argv[index + 1]
}

function positiveInteger(value, label) {
  const parsed = Number(value)
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a positive integer`)
  }
  return parsed
}

function sleep(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms)
  })
}

const logRateState = new Map()

function sanitizeLogText(value, limit = 2048) {
  return String(value)
    .replace(/https?:\/\/[^\s"'<>]+/gi, (rawUrl) => {
      try {
        const parsed = new URL(rawUrl)
        return `${parsed.origin}/<redacted>`
      } catch {
        return '<redacted-url>'
      }
    })
    .slice(0, limit)
}

function sanitizeLogValue(value) {
  if (typeof value === 'string') return sanitizeLogText(value)
  if (typeof value === 'bigint') return value.toString()
  if (Array.isArray(value)) return value.slice(0, 20).map(sanitizeLogValue)
  if (value && typeof value === 'object') {
    return Object.fromEntries(
      Object.entries(value).slice(0, 40).map(([key, item]) => [key, sanitizeLogValue(item)])
    )
  }
  return value
}

function emitLog(level, event, message, attributes = {}) {
  const severityNumbers = { DEBUG: 5, INFO: 9, WARN: 13, ERROR: 17 }
  const payload = {
    ...sanitizeLogValue(attributes),
    log_schema_version: 1,
    event: sanitizeLogText(event, 128),
    message: sanitizeLogText(message, 4096),
    level,
    SeverityText: level,
    SeverityNumber: severityNumbers[level],
  }
  const target = level === 'WARN' || level === 'ERROR' ? process.stderr : process.stdout
  target.write(`${JSON.stringify(payload)}\n`)
}

function emitLogEvery(intervalSeconds, level, event, message, attributes = {}) {
  const key = `${level}:${event}`
  const now = Date.now()
  const previous = logRateState.get(key)
  if (previous && now - previous.lastEmittedAt < Math.max(0, intervalSeconds) * 1000) {
    previous.suppressedCount += 1
    return
  }

  emitLog(level, event, message, {
    ...(previous?.suppressedCount ? { suppressed_count: previous.suppressedCount } : {}),
    ...attributes,
  })
  logRateState.set(key, { lastEmittedAt: now, suppressedCount: 0 })
}

function errorAttributes(error) {
  if (error instanceof Error) {
    return {
      error_type: error.name,
      error: sanitizeLogText(error.message),
    }
  }
  return { error: sanitizeLogText(error) }
}

export function validatePythPayload(payload) {
  const { updateData, publishTimes } = payload ?? {}
  if (!Array.isArray(updateData) || updateData.length === 0 || updateData.some(item =>
    typeof item !== 'string' || !/^(0x)?(?:[0-9a-f]{2})+$/i.test(item))) {
    throw new Error('Cached Pyth payload did not include valid updateData')
  }
  if (!Array.isArray(publishTimes) || publishTimes.length !== 6) {
    throw new Error('Cached Pyth payload must include six feed publish times')
  }
  return {
    updateData: updateData.map(item => item.startsWith('0x') ? item : `0x${item}`),
    publishTimes: publishTimes.map(value => positiveInteger(value, 'publishTime')),
    source: String(payload.source ?? 'database'),
  }
}

async function fetchCachedPythUpdate(backendUrl) {
  const response = await fetch(new URL('/api/perps/pyth/cached-latest', backendUrl), {
    signal: AbortSignal.timeout(10_000),
  })
  if (!response.ok) throw new Error(`Cached Pyth request failed with HTTP ${response.status}`)
  return validatePythPayload((await response.json())?.data)
}

/** One serial worker instance retains an unresolved transaction across health ticks. */
export function createOracleWorker({ account, backendUrl, dryRun = false, maxPayloadAgeSeconds = 50,
  publicClient, walletClient, feeds, pollSeconds = 30, now = Date.now,
  fetchPayload = () => fetchCachedPythUpdate(backendUrl), log = emitLog, logEvery = emitLogEvery,
}) {
  let nextRefreshAt = 0
  let retryAt = 0
  let failures = 0
  let pending
  let running = false
  const reconcile = async () => {
    const tx = pending
    // A timeout/RPC error retains the hash. Never send another transaction until resolved.
    const receipt = await publicClient.waitForTransactionReceipt({
      hash: tx.hash, timeout: 10_000,
      onReplaced: ({ transaction }) => { tx.hash = transaction.hash },
    })
    pending = undefined
    if (receipt.status !== 'success') throw new Error(`updateMarkPrice tx failed: ${tx.hash}`)
    const after = await readHealth(publicClient, feeds)
    const recovered = after.lag === 0n
    log('INFO', 'oracle_update_mined', 'Oracle mark-price update was mined', {
      transaction_hash: tx.hash, block_number: receipt.blockNumber, update_fee_wei: tx.fee,
      previous_mark_time: tx.markTime, mark_time: after.status.lastMarkTime,
      mark_price: after.status.lastMarkPrice, oracle_frozen: after.status.oracleFrozen,
      trading_active: after.status.tradingActive, repair: tx.repair,
      previous_lag_seconds: Number(tx.lag), lag_seconds: Number(after.lag), synchronized: recovered,
    })
    if (!recovered) logEvery(60, 'WARN', 'oracle_sync_pending', 'Stored Pyth feeds remain behind the engine mark', {
      lag_seconds: Number(after.lag), mark_time: after.status.lastMarkTime, oldest_publish_time: after.oldest,
    })
    return { status: recovered ? 'synchronized' : 'lagging', hash: tx.hash, lag: after.lag }
  }
  return async function iterate() {
    if (running) return { status: 'busy' }
    running = true
    try {
      if (pending) return await reconcile()
      if (now() < retryAt) return { status: 'backoff' }
      let before = await readHealth(publicClient, feeds)
      let repair = before.lag > 0n
      if (repair) logEvery(60, 'WARN', 'oracle_sync_lag', 'Stored Pyth feeds are behind the engine mark', {
        lag_seconds: Number(before.lag), mark_time: before.status.lastMarkTime, oldest_publish_time: before.oldest,
      })
      if (!repair && now() < nextRefreshAt) return { status: 'healthy' }
      const payload = validatePythPayload(await fetchPayload())
      // The payload may have advanced while the backend request was in flight.
      // Compare it with a block read AFTER that request, not an obsolete head.
      before = await readHealth(publicClient, feeds)
      repair = before.lag > 0n
      const minPublishTime = Math.min(...payload.publishTimes)
      const maxPublishTime = Math.max(...payload.publishTimes)
      const markTime = before.status.lastMarkTime
      const ageSeconds = Number(before.block.timestamp) - minPublishTime
      if (BigInt(minPublishTime) < markTime || (!repair && BigInt(minPublishTime) === markTime)) {
        nextRefreshAt = now() + pollSeconds * 1000
        logEvery(60, repair ? 'WARN' : 'INFO', 'oracle_update_not_needed', 'Cached payload cannot advance or repair this mark', {
          min_publish_time: minPublishTime, max_publish_time: maxPublishTime, onchain_mark_time: markTime,
          lag_seconds: Number(before.lag),
        })
        return { status: 'waiting_for_payload' }
      }
      if (ageSeconds > maxPayloadAgeSeconds || BigInt(maxPublishTime) > before.block.timestamp) {
        retryAt = now() + Math.min(30_000, 5_000 * 2 ** Math.min(failures++, 3))
        logEvery(60, 'WARN', 'oracle_update_payload_stale', 'Cached Pyth payload is outside the submission age window', {
          min_publish_time: minPublishTime, max_publish_time: maxPublishTime,
          payload_age_seconds: ageSeconds, max_payload_age_seconds: maxPayloadAgeSeconds,
        })
        return { status: 'stale_payload' }
      }
      const fee = await publicClient.readContract({ address: ADDRESSES.pletherOracle, abi: PLETHER_ORACLE_ABI,
        functionName: 'getUpdateFee', args: [payload.updateData] })
      if (dryRun) {
        log('INFO', 'oracle_update_dry_run', 'Oracle updater prepared a dry-run transaction', {
          min_publish_time: minPublishTime, update_fee_wei: fee, repair, lag_seconds: Number(before.lag),
        })
        return { status: 'dry_run' }
      }
      const balance = await publicClient.getBalance({ address: account.address })
      if (balance < fee) throw new Error(`Updater balance ${formatEther(balance)} ETH is below update fee ${formatEther(fee)} ETH`)
      const submissionHealth = await readHealth(publicClient, feeds)
      const submissionAge = Number(submissionHealth.block.timestamp) - minPublishTime
      if (submissionAge > maxPayloadAgeSeconds || BigInt(maxPublishTime) > submissionHealth.block.timestamp
        || BigInt(minPublishTime) < submissionHealth.status.lastMarkTime
        || (submissionHealth.lag === 0n && BigInt(minPublishTime) === submissionHealth.status.lastMarkTime)) {
        retryAt = now() + Math.min(30_000, 5_000 * 2 ** Math.min(failures++, 3))
        return { status: 'stale_payload' }
      }
      const { request } = await publicClient.simulateContract({ account, address: ADDRESSES.orderRouter,
        abi: ORDER_ROUTER_ABI, functionName: 'updateMarkPrice', args: [payload.updateData], value: fee })
      const hash = await walletClient.writeContract(request)
      nextRefreshAt = now() + pollSeconds * 1000
      retryAt = 0
      failures = 0
      pending = { hash, fee, repair, lag: before.lag, markTime }
      return await reconcile()
    } catch (error) {
      if (!pending) retryAt = now() + Math.min(30_000, 5_000 * 2 ** Math.min(failures++, 3))
      throw error
    } finally { running = false }
  }
}

async function main() {
  const args = new Set(process.argv.slice(2))
  const loop = args.has('--loop')
  const dryRun = process.env.DRY_RUN === 'true' || args.has('--dry-run')
  const rpcUrl = requiredEnv('ARBITRUM_SEPOLIA_RPC_URL', 'RPC_URL')
  const backendUrl = process.env.PERPS_ORACLE_UPDATER_BACKEND_URL ?? 'http://127.0.0.1:3001'
  const pollSeconds = positiveInteger(
    readFlag('--poll-seconds', process.env.PERPS_ORACLE_UPDATER_POLL_SECONDS ?? '30'),
    'poll seconds'
  )
  const healthPollSeconds = positiveInteger(
    readFlag('--health-poll-seconds', process.env.PERPS_ORACLE_UPDATER_HEALTH_POLL_SECONDS ?? '5'),
    'health poll seconds'
  )
  const maxPayloadAgeSeconds = positiveInteger(
    readFlag('--max-payload-age-seconds', process.env.PERPS_ORACLE_UPDATER_MAX_PAYLOAD_AGE_SECONDS ?? '50'),
    'max payload age seconds'
  )
  const privateKey = optionalPrivateKey()

  if (!dryRun && !privateKey) {
    throw new Error('PERPS_ORACLE_UPDATER_PRIVATE_KEY is required unless DRY_RUN=true or --dry-run is set')
  }

  const publicClient = createPublicClient({
    chain: arbitrumSepolia,
    transport: rpcTransport(rpcUrl),
  })
  const chainId = await publicClient.getChainId()
  if (chainId !== ARBITRUM_SEPOLIA_CHAIN_ID) {
    throw new Error(`Wrong RPC chain id: expected ${ARBITRUM_SEPOLIA_CHAIN_ID}, got ${chainId}`)
  }

  const account = privateKey ? privateKeyToAccount(privateKey) : undefined
  const walletClient = account
    ? createWalletClient({
      account,
      chain: arbitrumSepolia,
      transport: rpcTransport(rpcUrl),
    })
    : undefined

  const feeds = await loadOracleFeeds(publicClient)
  const run = createOracleWorker({ account, backendUrl, dryRun, maxPayloadAgeSeconds,
    publicClient, walletClient, feeds, pollSeconds })

  if (!loop) {
    await run()
    return
  }

  emitLog('INFO', 'oracle_worker_started', 'Cached Pyth oracle updater started', {
    chain_id: chainId,
    poll_seconds: pollSeconds,
    health_poll_seconds: healthPollSeconds,
    max_payload_age_seconds: maxPayloadAgeSeconds,
    dry_run: dryRun,
    updater_address: account?.address,
    backend_origin: new URL(backendUrl).origin,
  })
  while (true) {
    try {
      await run()
    } catch (error) {
      emitLogEvery(60, 'ERROR', 'oracle_worker_iteration_failed', 'Oracle updater iteration failed', errorAttributes(error))
    }
    await sleep(Math.min(pollSeconds, healthPollSeconds) * 1000)
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main().catch((error) => {
    emitLog('ERROR', 'oracle_worker_fatal', 'Oracle updater cannot start', errorAttributes(error))
    process.exitCode = 1
  })
}
