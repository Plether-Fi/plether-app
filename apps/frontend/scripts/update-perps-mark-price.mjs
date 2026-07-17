import { createPublicClient, createWalletClient, formatEther, http } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'

const ARBITRUM_SEPOLIA_CHAIN_ID = 421614

const ADDRESSES = {
  orderRouter: '0x04E3103752f623fBcDcD01f588590Af4c53E4c1E',
  pletherOracle: '0xADfEd3bf768D810309B97b4dF9F9E77Eaa3a401c',
  perpsPublicLens: '0x4E202C06e2C378d1a85577ac631e592AB66f23FB',
}

const PYTH_FEED_IDS = [
  '0xa995d00bb36a63cef7fd2c287dc105fc8f3d93779f062f09551b0af3e81ec30b',
  '0xef2c98c804ba503c6a707e38be4dfbb16683775f195b091252bf24693042fd52',
  '0x84c2dde9633d93d1bcad84e7dc41c9d56578b7ec52fabedc1f335d673df0a7c1',
  '0x3112b03a41c910ed446852aacf67118cb1bec67b2cd0b9a214c58cc0eaa2ecca',
  '0x8ccb376aa871517e807358d4e3cf0bc7fe4950474dbe6c9ffc21ef64e43fc676',
  '0x0b1e3297e69f162877b577b0d6a47a0d63b2392bc8499e6540da4187a63e28f8',
]

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

function requiredEnv(name) {
  const value = process.env[name]
  if (!value) {
    throw new Error(`${name} is required`)
  }
  return value
}

function optionalPrivateKey() {
  const value = process.env.PERPS_ORACLE_UPDATER_PRIVATE_KEY
  if (!value) return undefined
  return value.startsWith('0x') ? value : `0x${value}`
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

async function fetchPythUpdateData() {
  const hermesUrl = new URL(process.env.PYTH_HERMES_URL ?? 'https://hermes.pyth.network')
  hermesUrl.pathname = '/v2/updates/price/latest'
  hermesUrl.search = ''
  for (const id of PYTH_FEED_IDS) {
    hermesUrl.searchParams.append('ids[]', id)
  }

  const headers = {}
  if (process.env.PYTH_API_KEY) {
    headers.Authorization = `Bearer ${process.env.PYTH_API_KEY}`
  }

  const response = await fetch(hermesUrl, { headers })
  if (!response.ok) {
    throw new Error(`Hermes request failed: ${response.status} ${await response.text()}`)
  }

  const payload = await response.json()
  const updates = payload?.binary?.data
  if (!Array.isArray(updates) || updates.length === 0) {
    throw new Error('Hermes response did not include binary update data')
  }

  return updates.map((item) => item.startsWith('0x') ? item : `0x${item}`)
}

async function main() {
  const rpcUrl = requiredEnv('ARBITRUM_SEPOLIA_RPC_URL')
  const dryRun = process.env.DRY_RUN === 'true'
  const privateKey = optionalPrivateKey()

  if (!dryRun && !privateKey) {
    throw new Error('PERPS_ORACLE_UPDATER_PRIVATE_KEY is required unless DRY_RUN=true')
  }

  const publicClient = createPublicClient({
    chain: arbitrumSepolia,
    transport: http(rpcUrl),
  })

  const chainId = await publicClient.getChainId()
  if (chainId !== ARBITRUM_SEPOLIA_CHAIN_ID) {
    throw new Error(`Wrong RPC chain id: expected ${ARBITRUM_SEPOLIA_CHAIN_ID}, got ${chainId}`)
  }

  const before = await publicClient.readContract({
    address: ADDRESSES.perpsPublicLens,
    abi: PERPS_PUBLIC_LENS_ABI,
    functionName: 'getProtocolStatus',
  })
  console.log('Before:', formatStatus(before))

  const pythUpdateData = await fetchPythUpdateData()
  console.log(`Fetched ${pythUpdateData.length} Pyth update blob(s) for ${PYTH_FEED_IDS.length} feeds`)

  const updateFee = await publicClient.readContract({
    address: ADDRESSES.pletherOracle,
    abi: PLETHER_ORACLE_ABI,
    functionName: 'getUpdateFee',
    args: [pythUpdateData],
  })
  console.log(`Pyth update fee: ${formatEther(updateFee)} ETH`)

  if (dryRun) {
    console.log('DRY_RUN=true, not sending transaction')
    return
  }

  const account = privateKeyToAccount(privateKey)
  const walletClient = createWalletClient({
    account,
    chain: arbitrumSepolia,
    transport: http(rpcUrl),
  })

  const balance = await publicClient.getBalance({ address: account.address })
  if (balance < updateFee) {
    throw new Error(`Updater balance ${formatEther(balance)} ETH is below update fee ${formatEther(updateFee)} ETH`)
  }

  const { request } = await publicClient.simulateContract({
    account,
    address: ADDRESSES.orderRouter,
    abi: ORDER_ROUTER_ABI,
    functionName: 'updateMarkPrice',
    args: [pythUpdateData],
    value: updateFee,
  })

  const hash = await walletClient.writeContract(request)
  console.log(`Submitted updateMarkPrice tx: ${hash}`)

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

main().catch((error) => {
  console.error(error)
  process.exitCode = 1
})
