import { pathToFileURL } from 'node:url'
import { BaseError, ContractFunctionRevertedError, createPublicClient, createWalletClient, formatEther, http, parseAbi } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'

const ARBITRUM_SEPOLIA_CHAIN_ID = 421614

const ADDRESSES = {
  orderRouter: '0x918663d6da8cb33b325a04c4a5e8c51d42f3b444',
  pletherOracle: '0x4f2c62a0e03657f52c633c19c11f88d8f0897bb2',
  perpsPublicLens: '0xe1c9c8d3c068c49d933396c9591a55126f29d10e',
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

const ORACLE_HEALTH_ERRORS = parseAbi([
  'error PletherOracle__NoAcceptedBasket()',
  'error PletherOracle__PriceOutOfOrder(uint64 publishTime, uint64 lastMarkTime)',
  'error PletherOracle__EqualTimeMarkMismatch(uint64 publishTime, uint256 basketPrice, uint256 enginePrice)',
  'error PletherOracle__StalePrice(uint8 mode, bytes32 feedId, uint256 publishTime, uint256 maxStaleness, uint256 currentTimestamp)',
  'error PletherOracle__PublishTimeDivergence(uint8 mode, uint256 minPublishTime, uint256 maxPublishTime, uint256 maxDivergence)',
  'error PletherOracle__BasketConfidenceTooWide(uint8 mode, uint256 basketConfidence, uint256 basketPrice, uint256 maxConfidenceBps)',
  'error PletherOracle__InvalidPrice(bytes32 feedId, int64 price)',
  'error PletherOracle__InvalidInverseConfidence(bytes32 feedId, int64 price, uint64 confidence)',
  'error PletherOracle__ZeroBasketPrice()',
])
const RECOVERABLE_HEALTH_ERRORS = new Set(ORACLE_HEALTH_ERRORS.map(error => error.name))

const PLETHER_ORACLE_ABI = [
  ...ORACLE_HEALTH_ERRORS,
  ...parseAbi(['function pyth() view returns (address)', 'function pythFeedIds(uint256) view returns (bytes32)']),
  ...parseAbi([
    'function getAcceptedBasketMark() view returns (bool accepted, uint256 markPrice, uint64 publishTime)',
    'function getLatestPrice(uint8 mode) view returns ((uint256 price, uint256 markPrice, uint64 publishTime, uint256 updateFee, uint256 maxStaleness, bool closeOnly, bool oracleFrozen, bool isFadWindow) snapshot)',
  ]),
  {
    type: 'function',
    name: 'getUpdateFee',
    stateMutability: 'view',
    inputs: [{ name: 'pythUpdateData', type: 'bytes[]' }],
    outputs: [{ name: 'pythFee', type: 'uint256' }],
  },
]

const ORDER_ROUTER_ABI = [
  ...parseAbi(['error PletherOracle__BasketConfidenceTooWide(uint8 mode, uint256 basketConfidence, uint256 basketPrice, uint256 maxConfidenceBps)']),
  {
    type: 'function',
    name: 'updateMarkPrice',
    stateMutability: 'payable',
    inputs: [{ name: 'pythUpdateData', type: 'bytes[]' }],
    outputs: [],
  },
]

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

function decodedContractError(error) {
  return error instanceof BaseError
    ? error.walk(cause => cause instanceof ContractFunctionRevertedError)
    : undefined
}

async function readValidatedBasket(publicClient, blockNumber) {
  try {
    // MarkRefresh is the policy of the transaction this worker submits. In
    // frozen periods Core validates the engine mark instead of the live basket.
    const snapshot = await publicClient.readContract({ address: ADDRESSES.pletherOracle,
      abi: PLETHER_ORACLE_ABI, functionName: 'getLatestPrice', args: [1], blockNumber })
    if (!(snapshot.price > 0n && snapshot.markPrice > 0n && snapshot.publishTime > 0n)) {
      throw new Error('Oracle returned an invalid mark-refresh snapshot')
    }
    return { valid: true }
  } catch (error) {
    const reverted = decodedContractError(error)
    // A transport/ABI/unknown failure is not evidence that sending a paid update
    // can repair the oracle. Only decoded basket-validation failures permit it.
    if (!(reverted instanceof ContractFunctionRevertedError) ||
        !RECOVERABLE_HEALTH_ERRORS.has(reverted.data?.errorName)) throw error
    return { valid: false, validationError: error }
  }
}

async function readHealth(publicClient, block = undefined) {
  block ??= await publicClient.getBlock({ blockTag: 'latest' })
  const [status, [accepted, , acceptedPublishTime], validation] = await Promise.all([
    publicClient.readContract({ address: ADDRESSES.perpsPublicLens, abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'getProtocolStatus', blockNumber: block.number }),
    publicClient.readContract({ address: ADDRESSES.pletherOracle, abi: PLETHER_ORACLE_ABI,
      functionName: 'getAcceptedBasketMark', blockNumber: block.number }),
    readValidatedBasket(publicClient, block.number),
  ])
  // Core v2's accepted basket is independent of Pyth's mutable stored feeds.
  // Its newest component time is ordered against the engine mark; Core itself
  // validates every component's age, divergence, confidence and equal-time price.
  const lag = accepted && !status.oracleFrozen && status.lastMarkTime > acceptedPublishTime
    ? status.lastMarkTime - acceptedPublishTime : 0n
  return { status, block, accepted, acceptedPublishTime, lag, ...validation }
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
  publicClient, walletClient, pollSeconds = 30, now = Date.now,
  fetchPayload = () => fetchCachedPythUpdate(backendUrl), log = emitLog, logEvery = emitLogEvery,
}) {
  let nextRefreshAt = 0
  let retryAt = 0
  let failures = 0
  let pending
  let running = false
  const deferRetry = () => {
    retryAt = now() + Math.min(30_000, 5000 * 2 ** Math.min(failures++, 3))
  }
  const refreshHealth = async previous => {
    const block = await publicClient.getBlock({ blockTag: 'latest' })
    // Reuse pinned reads only for the identical block, including across reorgs.
    // Normally this adds one block lookup, not another three contract reads.
    if (block.hash && block.hash === previous.block.hash) return previous
    return readHealth(publicClient, block)
  }
  const checkPayload = (health, minPublishTime, maxPublishTime) => {
    if (health.status.oracleFrozen) {
      if (!health.valid) throw health.validationError
      return 'healthy'
    }
    const repair = !health.valid
    const markTime = health.status.lastMarkTime > health.acceptedPublishTime
      ? health.status.lastMarkTime : health.acceptedPublishTime
    // Core v2 orders by the newest component, while validating all components
    // independently in simulation. The oldest need not cover the engine mark.
    if (BigInt(maxPublishTime) < markTime || (!repair && BigInt(maxPublishTime) === markTime)) {
      logEvery(60, repair ? 'WARN' : 'INFO', 'oracle_update_not_needed', 'Cached payload cannot advance or repair this mark', {
        min_publish_time: minPublishTime, max_publish_time: maxPublishTime, onchain_mark_time: markTime,
        lag_seconds: Number(health.lag),
      })
      nextRefreshAt = now() + pollSeconds * 1000
      failures = 0
      retryAt = 0
      return 'waiting_for_payload'
    }
    const ageSeconds = Number(health.block.timestamp) - minPublishTime
    if (ageSeconds > maxPayloadAgeSeconds || BigInt(maxPublishTime) > health.block.timestamp) {
      logEvery(60, 'WARN', 'oracle_update_payload_stale', 'Cached Pyth payload is outside the submission age window', {
        min_publish_time: minPublishTime, max_publish_time: maxPublishTime,
        payload_age_seconds: ageSeconds, max_payload_age_seconds: maxPayloadAgeSeconds,
      })
      deferRetry()
      return 'stale_payload'
    }
    return undefined
  }
  const reconcile = async () => {
    const tx = pending
    // A timeout/RPC error retains the hash. Never send another transaction until resolved.
    const receipt = await publicClient.waitForTransactionReceipt({
      hash: tx.hash, timeout: 10_000,
      onReplaced: ({ transaction }) => { tx.hash = transaction.hash },
    })
    pending = undefined
    if (receipt.status !== 'success') throw new Error(`updateMarkPrice tx failed: ${tx.hash}`)
    const after = await readHealth(publicClient)
    const recovered = after.valid
    log('INFO', 'oracle_update_mined', 'Oracle mark-price update was mined', {
      transaction_hash: tx.hash, block_number: receipt.blockNumber, update_fee_wei: tx.fee,
      previous_mark_time: tx.markTime, mark_time: after.status.lastMarkTime,
      mark_price: after.status.lastMarkPrice, oracle_frozen: after.status.oracleFrozen,
      trading_active: after.status.tradingActive, repair: tx.repair,
      previous_lag_seconds: Number(tx.lag), lag_seconds: Number(after.lag), synchronized: recovered,
    })
    if (!recovered) logEvery(60, 'WARN', 'oracle_sync_pending', 'Accepted oracle basket still fails mark-refresh validation', {
      lag_seconds: Number(after.lag), mark_time: after.status.lastMarkTime, accepted_publish_time: after.acceptedPublishTime,
    })
    return { status: recovered ? 'synchronized' : 'lagging', hash: tx.hash, lag: after.lag }
  }
  return async function iterate() {
    if (running) return { status: 'busy' }
    running = true
    try {
      if (pending) return await reconcile()
      if (now() < retryAt) return { status: 'backoff' }
      let before = await readHealth(publicClient)
      if (before.status.oracleFrozen) {
        if (!before.valid) throw before.validationError
        return { status: 'healthy' }
      }
      let repair = !before.valid
      if (repair) logEvery(60, 'WARN', 'oracle_sync_lag', 'Accepted oracle basket fails mark-refresh validation', {
        lag_seconds: Number(before.lag), mark_time: before.status.lastMarkTime, accepted_publish_time: before.acceptedPublishTime,
      })
      if (!repair && now() < nextRefreshAt) return { status: 'healthy' }
      const payload = validatePythPayload(await fetchPayload())
      const minPublishTime = Math.min(...payload.publishTimes)
      const maxPublishTime = Math.max(...payload.publishTimes)
      before = await refreshHealth(before)
      let rejection = checkPayload(before, minPublishTime, maxPublishTime)
      if (rejection) return { status: rejection }
      const fee = await publicClient.readContract({ address: ADDRESSES.pletherOracle, abi: PLETHER_ORACLE_ABI,
        functionName: 'getUpdateFee', args: [payload.updateData] })
      if (dryRun) {
        nextRefreshAt = now() + pollSeconds * 1000
        failures = 0
        retryAt = 0
        log('INFO', 'oracle_update_dry_run', 'Oracle updater prepared a dry-run transaction', {
          min_publish_time: minPublishTime, update_fee_wei: fee, repair: !before.valid, lag_seconds: Number(before.lag),
        })
        return { status: 'dry_run' }
      }
      const balance = await publicClient.getBalance({ address: account.address })
      if (balance < fee) throw new Error(`Updater balance ${formatEther(balance)} ETH is below update fee ${formatEther(fee)} ETH`)
      // Fee/balance RPCs can be slow. Recheck immediately before simulation;
      // never relax the future-time or oldest-component guards to recover.
      before = await refreshHealth(before)
      rejection = checkPayload(before, minPublishTime, maxPublishTime)
      if (rejection) return { status: rejection }
      repair = !before.valid
      const markTime = before.status.lastMarkTime
      let request
      try {
        const simulation = await publicClient.simulateContract({ account, address: ADDRESSES.orderRouter,
          abi: ORDER_ROUTER_ABI, functionName: 'updateMarkPrice', args: [payload.updateData], value: fee })
        request = simulation.request
      } catch (error) {
        const reverted = decodedContractError(error)
        if (!(reverted instanceof ContractFunctionRevertedError) ||
            reverted.data?.errorName !== 'PletherOracle__BasketConfidenceTooWide') throw error
        const [mode, basketConfidence, basketPrice, maxConfidenceBps] = reverted.data.args
        deferRetry()
        logEvery(60, 'WARN', 'oracle_update_confidence_too_wide', 'Pyth basket confidence exceeds the contract limit; waiting for a narrower payload', {
          price_mode: mode, basket_confidence: basketConfidence, basket_price: basketPrice,
          confidence_ratio_bps: Number(basketConfidence) * 10_000 / Number(basketPrice),
          max_confidence_bps: maxConfidenceBps, min_publish_time: minPublishTime, max_publish_time: maxPublishTime,
        })
        return { status: 'confidence_too_wide' }
      }
      const hash = await walletClient.writeContract(request)
      nextRefreshAt = now() + pollSeconds * 1000
      failures = 0
      retryAt = 0
      pending = { hash, fee, repair, lag: before.lag, markTime }
      return await reconcile()
    } catch (error) {
      if (!pending) deferRetry()
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

  await loadOracleFeeds(publicClient)
  const run = createOracleWorker({ account, backendUrl, dryRun, maxPayloadAgeSeconds,
    publicClient, walletClient, pollSeconds })

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
