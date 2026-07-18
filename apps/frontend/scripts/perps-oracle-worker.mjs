import { createPublicClient, createWalletClient, http } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'

const ARBITRUM_SEPOLIA_CHAIN_ID = 421614

const ADDRESSES = {
  orderRouter: '0x4A0a6c028164A1254e10C3e39cc89Af45090069e',
  pletherOracle: '0x8c95f554D728215b9f8D15b5F3Da5F5CD7Ba08bA',
  perpsPublicLens: '0xDdDCfb123569774427802fcA9D19CBF00c14e2Ad',
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
        ],
      },
    ],
  },
]

const PLETHER_ORACLE_ABI = [
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

function requiredEnv(...names) {
  for (const name of names) {
    const value = process.env[name]
    if (value) return value
  }
  throw new Error(`${names.join(' or ')} is required`)
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
  const parsed = Number.parseInt(String(value), 10)
  if (!Number.isFinite(parsed) || parsed <= 0) {
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

function formatStatus(status) {
  return {
    phase: Number(status.phase),
    lastMarkPrice: status.lastMarkPrice.toString(),
    lastMarkTime: status.lastMarkTime.toString(),
    oracleFrozen: status.oracleFrozen,
    fadWindow: status.fadWindow,
    tradingActive: status.tradingActive,
    withdrawalLive: status.withdrawalLive,
  }
}

async function fetchCachedPythUpdate(backendUrl) {
  const url = new URL('/api/perps/pyth/cached-latest', backendUrl)
  const response = await fetch(url)
  if (!response.ok) {
    throw new Error(`Cached Pyth request failed with HTTP ${response.status}`)
  }

  const payload = await response.json()
  const updateData = payload?.data?.updateData
  const publishTimes = payload?.data?.publishTimes
  if (!Array.isArray(updateData) || updateData.length === 0) {
    throw new Error('Cached Pyth payload did not include updateData')
  }
  if (!Array.isArray(publishTimes) || publishTimes.length === 0) {
    throw new Error('Cached Pyth payload did not include publishTimes')
  }

  return {
    updateData: updateData.map((item) => item.startsWith('0x') ? item : `0x${item}`),
    publishTimes: publishTimes.map((value) => positiveInteger(value, 'publishTime')),
    fetchedAt: Number(payload.data.fetchedAt),
    source: String(payload.data.source ?? 'database'),
  }
}

async function updateMarkFromCache({ account, backendUrl, dryRun, maxPayloadAgeSeconds, publicClient, walletClient }) {
  const before = await publicClient.readContract({
    address: ADDRESSES.perpsPublicLens,
    abi: PERPS_PUBLIC_LENS_ABI,
    functionName: 'getProtocolStatus',
  })
  const beforeStatus = formatStatus(before)

  const pythPayload = await fetchCachedPythUpdate(backendUrl)
  const maxPublishTime = Math.max(...pythPayload.publishTimes)
  const minPublishTime = Math.min(...pythPayload.publishTimes)
  const now = Math.floor(Date.now() / 1000)
  const ageSeconds = now - maxPublishTime

  if (BigInt(maxPublishTime) <= before.lastMarkTime) {
    emitLogEvery(300, 'INFO', 'oracle_update_not_needed', 'Cached Pyth payload is not newer than the on-chain mark', {
      payload_source: pythPayload.source,
      min_publish_time: minPublishTime,
      max_publish_time: maxPublishTime,
      onchain_mark_time: before.lastMarkTime,
      payload_age_seconds: ageSeconds,
    })
    return
  }

  if (ageSeconds > maxPayloadAgeSeconds) {
    emitLogEvery(60, 'WARN', 'oracle_update_payload_stale', 'Cached Pyth payload is too old to submit', {
      payload_source: pythPayload.source,
      max_publish_time: maxPublishTime,
      payload_age_seconds: ageSeconds,
      max_payload_age_seconds: maxPayloadAgeSeconds,
    })
    return
  }

  const updateFee = await publicClient.readContract({
    address: ADDRESSES.pletherOracle,
    abi: PLETHER_ORACLE_ABI,
    functionName: 'getUpdateFee',
    args: [pythPayload.updateData],
  })

  if (dryRun) {
    emitLogEvery(300, 'INFO', 'oracle_update_dry_run', 'Oracle updater prepared a dry-run transaction', {
      payload_source: pythPayload.source,
      min_publish_time: minPublishTime,
      max_publish_time: maxPublishTime,
      payload_age_seconds: ageSeconds,
      update_fee_wei: updateFee,
    })
    return
  }

  const balance = await publicClient.getBalance({ address: account.address })
  if (balance < updateFee) {
    throw new Error(`Updater balance ${formatEther(balance)} ETH is below update fee ${formatEther(updateFee)} ETH`)
  }

  const { request } = await publicClient.simulateContract({
    account,
    address: ADDRESSES.orderRouter,
    abi: ORDER_ROUTER_ABI,
    functionName: 'updateMarkPrice',
    args: [pythPayload.updateData],
    value: updateFee,
  })

  const hash = await walletClient.writeContract(request)

  const receipt = await publicClient.waitForTransactionReceipt({ hash })
  if (receipt.status !== 'success') {
    throw new Error(`updateMarkPrice tx failed: ${hash}`)
  }

  const after = await publicClient.readContract({
    address: ADDRESSES.perpsPublicLens,
    abi: PERPS_PUBLIC_LENS_ABI,
    functionName: 'getProtocolStatus',
  })
  const afterStatus = formatStatus(after)
  emitLogEvery(300, 'INFO', 'oracle_update_mined', 'Oracle mark-price update was mined', {
    transaction_hash: hash,
    block_number: receipt.blockNumber,
    payload_source: pythPayload.source,
    min_publish_time: minPublishTime,
    max_publish_time: maxPublishTime,
    payload_age_seconds: ageSeconds,
    update_fee_wei: updateFee,
    previous_mark_time: beforeStatus.lastMarkTime,
    mark_time: afterStatus.lastMarkTime,
    mark_price: afterStatus.lastMarkPrice,
    oracle_frozen: afterStatus.oracleFrozen,
    trading_active: afterStatus.tradingActive,
  })
}

async function main() {
  const args = new Set(process.argv.slice(2))
  const loop = args.has('--loop')
  const dryRun = process.env.DRY_RUN === 'true' || args.has('--dry-run')
  const rpcUrl = requiredEnv('ARBITRUM_SEPOLIA_RPC_URL', 'RPC_URL')
  const backendUrl = process.env.PERPS_ORACLE_UPDATER_BACKEND_URL ?? 'http://127.0.0.1:3001'
  const pollSeconds = positiveInteger(
    readFlag('--poll-seconds', process.env.PERPS_ORACLE_UPDATER_POLL_SECONDS ?? '300'),
    'poll seconds'
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
    transport: http(rpcUrl),
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
      transport: http(rpcUrl),
    })
    : undefined

  const run = async () => {
    await updateMarkFromCache({
      account,
      backendUrl,
      dryRun,
      maxPayloadAgeSeconds,
      publicClient,
      walletClient,
    })
  }

  if (!loop) {
    await run()
    return
  }

  emitLog('INFO', 'oracle_worker_started', 'Cached Pyth oracle updater started', {
    chain_id: chainId,
    poll_seconds: pollSeconds,
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
    await sleep(pollSeconds * 1000)
  }
}

main().catch((error) => {
  emitLog('ERROR', 'oracle_worker_fatal', 'Oracle updater cannot start', errorAttributes(error))
  process.exitCode = 1
})
