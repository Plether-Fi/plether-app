import { createPublicClient, createWalletClient, formatEther, http } from 'viem'
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
    throw new Error(`Cached Pyth request failed: ${response.status} ${await response.text()}`)
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
  console.log('Before:', beforeStatus)

  const pythPayload = await fetchCachedPythUpdate(backendUrl)
  const maxPublishTime = Math.max(...pythPayload.publishTimes)
  const minPublishTime = Math.min(...pythPayload.publishTimes)
  const now = Math.floor(Date.now() / 1000)
  const ageSeconds = now - maxPublishTime
  console.log(
    `Using cached ${pythPayload.source} Pyth payload ${minPublishTime}..${maxPublishTime}; age ${ageSeconds}s`
  )

  if (BigInt(maxPublishTime) <= before.lastMarkTime) {
    console.log(
      `Skipping update: cached publish time ${maxPublishTime} is not newer than onchain mark time ${before.lastMarkTime.toString()}.`
    )
    return
  }

  if (ageSeconds > maxPayloadAgeSeconds) {
    console.log(
      `Skipping update: cached payload age ${ageSeconds}s exceeds limit ${maxPayloadAgeSeconds}s. Keep plether-basket-worker --latest-loop running.`
    )
    return
  }

  const updateFee = await publicClient.readContract({
    address: ADDRESSES.pletherOracle,
    abi: PLETHER_ORACLE_ABI,
    functionName: 'getUpdateFee',
    args: [pythPayload.updateData],
  })
  console.log(`Pyth update fee: ${formatEther(updateFee)} ETH`)

  if (dryRun) {
    console.log('DRY_RUN=true, not sending transaction')
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
  console.log(`Submitted cached updateMarkPrice tx: ${hash}`)

  const receipt = await publicClient.waitForTransactionReceipt({ hash })
  if (receipt.status !== 'success') {
    throw new Error(`updateMarkPrice tx failed: ${hash}`)
  }
  console.log(`Confirmed in block ${receipt.blockNumber}`)

  const after = await publicClient.readContract({
    address: ADDRESSES.perpsPublicLens,
    abi: PERPS_PUBLIC_LENS_ABI,
    functionName: 'getProtocolStatus',
  })
  console.log('After:', formatStatus(after))
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

  console.log(`Starting cached oracle updater loop every ${pollSeconds}s using ${backendUrl}`)
  while (true) {
    try {
      await run()
    } catch (error) {
      console.error(error)
    }
    await sleep(pollSeconds * 1000)
  }
}

main().catch((error) => {
  console.error(error)
  process.exitCode = 1
})
