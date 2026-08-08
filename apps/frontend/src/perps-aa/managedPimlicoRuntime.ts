import { createSmartAccountClient } from 'permissionless'
import { SimpleSmartAccount } from 'permissionless/accounts/simple'
import { createPimlicoClient } from 'permissionless/clients/pimlico'
import {
  getAddress,
  hexToBigInt,
  http,
  isAddress,
  isAddressEqual,
  isHex,
  maxUint256,
  parseAbi,
  parseAbiItem,
  size,
  toEventSelector,
  type Account,
  type Address,
  type Chain,
  type Hex,
  type PublicClient,
  type Transport,
  type WalletClient,
} from 'viem'
import {
  getUserOperationHash,
  type UserOperationReceipt,
} from 'viem/account-abstraction'
import { arbitrumSepolia } from 'viem/chains'
import type { PerpsAaDeploymentManifest } from './manifest'
import type {
  ManagedUserOperation,
  PerpsAaSmartAccountRuntime,
  PimlicoUserOperationStatus,
  PimlicoUserOperationStatusResult,
  SponsoredOperationRecoverySnapshot,
} from './runtimeContext'
import { UserOperationReceiptNotSafeError } from './runtimeContext'

const PIMLICO_STATUSES = new Set<PimlicoUserOperationStatus>([
  'not_found',
  'not_submitted',
  'submitted',
  'queued',
  'rejected',
  'reverted',
  'included',
  'failed',
])

const ENTRY_POINT_NONCE_ABI = parseAbi([
  'function getNonce(address sender, uint192 key) view returns (uint256 nonce)',
])
const USER_OPERATION_EVENT = parseAbiItem(
  'event UserOperationEvent(bytes32 indexed userOpHash, address indexed sender, address indexed paymaster, uint256 nonce, bool success, uint256 actualGasCost, uint256 actualGasUsed)'
)
const USER_OPERATION_EVENT_SELECTOR = toEventSelector(USER_OPERATION_EVENT)
const ARBITRUM_SEPOLIA_BLOCKSCOUT_ORIGIN =
  'https://arbitrum-sepolia.blockscout.com'

interface CreateManagedPimlicoRuntimeInput {
  manifest: PerpsAaDeploymentManifest
  ownerAddress: Address
  walletClient: WalletClient
  publicClient: PublicClient
}

function parseStatus(value: unknown): PimlicoUserOperationStatusResult {
  if (!value || typeof value !== 'object') {
    throw new Error('Pimlico returned an invalid UserOperation status')
  }
  const result = value as {
    status?: unknown
    transactionHash?: unknown
  }
  if (
    typeof result.status !== 'string' ||
    !PIMLICO_STATUSES.has(result.status as PimlicoUserOperationStatus)
  ) {
    throw new Error('Pimlico returned an unknown UserOperation status')
  }
  if (
    result.transactionHash !== null &&
    (
      typeof result.transactionHash !== 'string' ||
      !isHex(result.transactionHash, { strict: true }) ||
      size(result.transactionHash) !== 32
    )
  ) {
    throw new Error('Pimlico returned an invalid transaction hash')
  }
  return {
    status: result.status as PimlicoUserOperationStatus,
    transactionHash: result.transactionHash,
  }
}

function assertReceipt(
  receipt: UserOperationReceipt<'0.8'>,
  expectedHash: `0x${string}`,
  expectedSender: Address,
  expectedEntryPoint: Address
): void {
  if (
    receipt.userOpHash.toLowerCase() !== expectedHash.toLowerCase() ||
    !isAddressEqual(receipt.sender, expectedSender) ||
    !isAddressEqual(receipt.entryPoint, expectedEntryPoint)
  ) {
    throw new Error(
      'Pimlico returned a receipt for a different Trading Account operation'
    )
  }
}

function normalizeReceiptNonce(
  receipt: UserOperationReceipt<'0.8'>
): UserOperationReceipt<'0.8'> {
  // Pimlico's JSON-RPC response currently leaves `nonce` as a quantity string
  // at runtime even though permissionless/viem declares it as bigint.
  const nonce: unknown = receipt.nonce
  if (
    typeof nonce === 'bigint' &&
    nonce >= 0n &&
    nonce <= maxUint256
  ) {
    return receipt
  }
  if (
    typeof nonce === 'string' &&
    /^0x(?:0|[1-9a-f][0-9a-f]*)$/i.test(nonce)
  ) {
    const normalizedNonce = hexToBigInt(nonce as Hex)
    if (normalizedNonce > maxUint256) {
      throw new Error('Pimlico returned an invalid UserOperation receipt nonce')
    }
    return {
      ...receipt,
      nonce: normalizedNonce,
    }
  }
  throw new Error('Pimlico returned an invalid UserOperation receipt nonce')
}

async function assertCanonicalSafeReceipt(input: {
  publicClient: PublicClient
  receipt: UserOperationReceipt<'0.8'>
  expectedHash: Hex
  expectedSender: Address
  entryPoint: Address
}): Promise<void> {
  const [safeBlock, canonicalTransactionReceipt] = await Promise.all([
    input.publicClient.getBlock({ blockTag: 'safe' }),
    input.publicClient.getTransactionReceipt({
      hash: input.receipt.receipt.transactionHash,
    }),
  ])
  if (
    canonicalTransactionReceipt.blockNumber !==
      input.receipt.receipt.blockNumber ||
    canonicalTransactionReceipt.blockHash.toLowerCase() !==
      input.receipt.receipt.blockHash.toLowerCase() ||
    canonicalTransactionReceipt.transactionHash.toLowerCase() !==
      input.receipt.receipt.transactionHash.toLowerCase() ||
    canonicalTransactionReceipt.status !== input.receipt.receipt.status
  ) {
    throw new Error(
      'Pimlico returned a noncanonical UserOperation transaction receipt'
    )
  }

  const events = await input.publicClient.getLogs({
    address: input.entryPoint,
    event: USER_OPERATION_EVENT,
    args: {
      userOpHash: input.expectedHash,
      sender: input.expectedSender,
    },
    fromBlock: input.receipt.receipt.blockNumber,
    toBlock: input.receipt.receipt.blockNumber,
  })
  if (events.length !== 1) {
    throw new Error(
      'The canonical transaction does not contain the expected UserOperation'
    )
  }
  const [event] = events
  if (
    event.blockHash.toLowerCase() !==
      input.receipt.receipt.blockHash.toLowerCase() ||
    event.transactionHash.toLowerCase() !==
      input.receipt.receipt.transactionHash.toLowerCase() ||
    event.args.userOpHash?.toLowerCase() !==
      input.expectedHash.toLowerCase() ||
    !event.args.sender ||
    !isAddressEqual(event.args.sender, input.expectedSender) ||
    event.args.nonce !== input.receipt.nonce ||
    event.args.success !== input.receipt.success
  ) {
    throw new Error(
      'The canonical UserOperation event does not match Pimlico’s receipt'
    )
  }
  if (input.receipt.receipt.blockNumber > safeBlock.number) {
    throw new UserOperationReceiptNotSafeError(input.receipt)
  }
}

function recordOf(value: unknown): Record<string, unknown> | undefined {
  return value && typeof value === 'object'
    ? value as Record<string, unknown>
    : undefined
}

function isExpectedAddress(value: unknown, expected: Address): boolean {
  return typeof value === 'string' &&
    isAddress(value, { strict: true }) &&
    isAddressEqual(value, expected)
}

function parseBlockNumber(value: unknown, field: string): bigint {
  if (
    typeof value === 'number' &&
    Number.isSafeInteger(value) &&
    value >= 0
  ) {
    return BigInt(value)
  }
  if (
    typeof value !== 'string' ||
    !(
      /^(0|[1-9]\d*)$/.test(value) ||
      /^0x(?:0|[1-9a-f][0-9a-f]*)$/i.test(value)
    )
  ) {
    throw new Error(`Blockscout returned an invalid ${field}`)
  }
  return value.startsWith('0x') || value.startsWith('0X')
    ? hexToBigInt(value as Hex)
    : BigInt(value)
}

function parseHash(value: unknown, field: string): Hex {
  if (
    typeof value !== 'string' ||
    !isHex(value, { strict: true }) ||
    size(value) !== 32
  ) {
    throw new Error(`Blockscout returned an invalid ${field}`)
  }
  return value
}

function expectedAddressTopic(address: Address): Hex {
  return `0x${'0'.repeat(24)}${address.slice(2).toLowerCase()}`
}

type BlockscoutUserOperationEventLocator =
  | {
      kind: 'included'
      blockNumber: bigint
      transactionHash: Hex
    }
  | {
      kind: 'not-located'
    }

async function getBlockscoutUserOperationEventLocator(input: {
  userOperationHash: Hex
  safeBlockNumber: bigint
  accountAddress: Address
  entryPoint: Address
}): Promise<BlockscoutUserOperationEventLocator> {
  const url = new URL(`${ARBITRUM_SEPOLIA_BLOCKSCOUT_ORIGIN}/api`)
  url.search = new URLSearchParams({
    module: 'logs',
    action: 'getLogs',
    fromBlock: '0',
    toBlock: input.safeBlockNumber.toString(),
    address: input.entryPoint,
    topic0: USER_OPERATION_EVENT_SELECTOR,
    topic1: input.userOperationHash,
    topic0_1_opr: 'and',
  }).toString()
  const response = await fetch(
    url,
    {
      headers: { accept: 'application/json' },
      signal: AbortSignal.timeout(10_000),
    }
  )
  if (!response.ok) {
    throw new Error('Blockscout UserOperation event lookup is unavailable')
  }

  const value: unknown = await response.json()
  const result = recordOf(value)
  if (!Array.isArray(result?.result)) {
    throw new Error('Blockscout returned invalid UserOperation event data')
  }
  if (
    result.status === '0' &&
    result.message === 'No logs found' &&
    result.result.length === 0
  ) {
    // Explorer misses are never treated as proof of onchain absence. The
    // locator is used only to find a block for a bounded canonical RPC check.
    return { kind: 'not-located' }
  }
  if (
    result.status !== '1' ||
    result.message !== 'OK' ||
    result.result.length !== 1
  ) {
    throw new Error('Blockscout returned ambiguous UserOperation event data')
  }

  const event = recordOf(result.result[0])
  const topics = event?.topics
  if (!Array.isArray(topics) || topics.length !== 4) {
    throw new Error('Blockscout returned invalid UserOperation topics')
  }
  const selectorTopic = parseHash(topics[0], 'UserOperation event selector')
  const userOperationHashTopic = parseHash(
    topics[1],
    'UserOperation hash topic'
  )
  const senderTopic = parseHash(topics[2], 'UserOperation sender topic')
  parseHash(topics[3], 'UserOperation paymaster topic')
  const blockNumber = parseBlockNumber(
    event?.blockNumber,
    'UserOperation block'
  )
  const transactionHash = parseHash(
    event?.transactionHash,
    'UserOperation transaction hash'
  )
  if (
    blockNumber > input.safeBlockNumber ||
    !isExpectedAddress(event?.address, input.entryPoint) ||
    selectorTopic.toLowerCase() !== USER_OPERATION_EVENT_SELECTOR.toLowerCase() ||
    userOperationHashTopic.toLowerCase() !==
      input.userOperationHash.toLowerCase() ||
    senderTopic.toLowerCase() !== expectedAddressTopic(input.accountAddress)
  ) {
    throw new Error('Blockscout returned mismatched UserOperation evidence')
  }
  return {
    kind: 'included',
    transactionHash,
    blockNumber,
  }
}

export async function createManagedPimlicoRuntime({
  manifest,
  ownerAddress,
  walletClient,
  publicClient,
}: CreateManagedPimlicoRuntimeInput): Promise<PerpsAaSmartAccountRuntime> {
  if (
    manifest.chainId !== arbitrumSepolia.id
  ) {
    throw new Error('The manifest does not describe the supported Pimlico account stack')
  }
  if (
    publicClient.chain?.id !== manifest.chainId ||
    walletClient.chain?.id !== manifest.chainId ||
    !walletClient.account ||
    !isAddressEqual(walletClient.account.address, ownerAddress)
  ) {
    throw new Error('The connected wallet is not ready on the manifest chain')
  }

  const owner = walletClient as WalletClient<
    Transport,
    Chain | undefined,
    Account
  >
  const accountIndex = BigInt(manifest.smartAccountIndex)
  const smartAccount = await SimpleSmartAccount.toSimpleSmartAccount({
    client: publicClient,
    owner,
    entryPoint: {
      address: manifest.entryPoint,
      version: manifest.entryPointVersion,
    },
    factoryAddress: manifest.smartAccountFactory,
    index: accountIndex,
    nonceKey: 0n,
  })
  const pimlicoClient = createPimlicoClient({
    chain: arbitrumSepolia,
    entryPoint: {
      address: manifest.entryPoint,
      version: manifest.entryPointVersion,
    },
    transport: http(manifest.pimlicoRpcUrl),
  })
  const smartAccountClient = createSmartAccountClient({
    account: smartAccount,
    chain: arbitrumSepolia,
    client: publicClient,
    bundlerTransport: http(manifest.pimlicoRpcUrl),
    paymaster: pimlicoClient,
    paymasterContext: {},
    userOperation: {
      estimateFeesPerGas: async () =>
        (await pimlicoClient.getUserOperationGasPrice()).fast,
    },
  })
  const accountAddress = getAddress(smartAccount.address)

  return {
    chainId: manifest.chainId,
    ownerAddress: getAddress(ownerAddress),
    factoryAddress: getAddress(manifest.smartAccountFactory),
    accountVersion: manifest.smartAccountVersion,
    accountIndex: manifest.smartAccountIndex,
    manifestVersion: manifest.version,
    verifyObservedInclusion: async (inclusion) => {
      try {
        const block = await publicClient.getBlock({
          blockNumber: inclusion.blockNumber,
        })
        return block.hash.toLowerCase() === inclusion.blockHash.toLowerCase()
          ? 'canonical'
          : 'reorged'
      } catch {
        // An unavailable or lagging RPC cannot prove that an observed block
        // was reorged. Keep the conservative inclusion state and retry later.
        return 'unknown'
      }
    },
    getRecoverySnapshot: async (userOperationHash, nonceKey = 0n) => {
      const block = await publicClient.getBlock({ blockTag: 'safe' })
      const [accountNonce, locator] = await Promise.all([
        publicClient.readContract({
          address: manifest.entryPoint,
          abi: ENTRY_POINT_NONCE_ABI,
          functionName: 'getNonce',
          args: [accountAddress, nonceKey],
          blockNumber: block.number,
        }),
        getBlockscoutUserOperationEventLocator({
          userOperationHash,
          safeBlockNumber: block.number,
          accountAddress,
          entryPoint: manifest.entryPoint,
        }).catch(() => ({ kind: 'not-located' } as const)),
      ])
      let userOperationEvidence:
        SponsoredOperationRecoverySnapshot['userOperationEvidence'] = {
          kind: 'not-located',
        }
      if (locator.kind === 'included') {
        // Blockscout is only the full-history locator. A bounded one-block
        // query against the configured chain RPC is the authoritative
        // inclusion proof and supplies the execution outcome.
        try {
          const events = await publicClient.getLogs({
            address: manifest.entryPoint,
            event: USER_OPERATION_EVENT,
            args: {
              userOpHash: userOperationHash,
              sender: accountAddress,
            },
            fromBlock: locator.blockNumber,
            toBlock: locator.blockNumber,
          })
          if (events.length !== 1) {
            throw new Error(
              'The canonical chain does not contain the indexed UserOperation'
            )
          }
          const [event] = events
          if (
            event.blockNumber !== locator.blockNumber ||
            event.transactionHash.toLowerCase() !==
              locator.transactionHash.toLowerCase() ||
            event.args.userOpHash?.toLowerCase() !==
              userOperationHash.toLowerCase() ||
            !event.args.sender ||
            !isAddressEqual(event.args.sender, accountAddress) ||
            typeof event.args.success !== 'boolean'
          ) {
            throw new Error(
              'The canonical chain returned mismatched UserOperation evidence'
            )
          }
          userOperationEvidence = {
            kind: 'included',
            success: event.args.success,
            transactionHash: event.transactionHash,
            blockNumber: event.blockNumber,
          }
        } catch {
          // A positive explorer row is only a locator. If its one-block
          // canonical proof fails, nonce/expiry recovery may still resolve
          // the future-execution risk, but inclusion remains unknown.
        }
      }
      return {
        blockNumber: block.number,
        blockTimestamp: block.timestamp,
        accountNonce,
        userOperationEvidence,
      }
    },
    smartAccount: {
      accountAddress,
      entryPoint: getAddress(manifest.entryPoint),

      prepareUserOperation: async ({ calls }) => {
        const operation = await smartAccountClient.prepareUserOperation({
          account: smartAccount,
          calls: calls.map((call) => ({
            to: call.to,
            value: call.value,
            data: call.data,
          })),
        })
        return operation as ManagedUserOperation
      },

      signUserOperation: async (operation) => ({
        ...operation,
        signature: await smartAccount.signUserOperation({
          ...operation,
          chainId: manifest.chainId,
        }),
      }),

      getUserOperationHash: (operation) =>
        getUserOperationHash({
          chainId: manifest.chainId,
          entryPointAddress: manifest.entryPoint,
          entryPointVersion: manifest.entryPointVersion,
          userOperation: operation,
        }),

      sendUserOperation: (operation) =>
        pimlicoClient.sendUserOperation({
          ...operation,
          entryPointAddress: manifest.entryPoint,
        }),

      getUserOperationStatus: async (userOperationHash) =>
        parseStatus(
          await pimlicoClient.getUserOperationStatus({
            hash: userOperationHash,
          })
        ),

      getUserOperationReceipt: async (userOperationHash) => {
        const receipt = normalizeReceiptNonce(
          await pimlicoClient.getUserOperationReceipt({
            hash: userOperationHash,
          })
        )
        assertReceipt(
          receipt,
          userOperationHash,
          accountAddress,
          manifest.entryPoint
        )
        await assertCanonicalSafeReceipt({
          publicClient,
          receipt,
          expectedHash: userOperationHash,
          expectedSender: accountAddress,
          entryPoint: manifest.entryPoint,
        })
        return receipt
      },
    },
  }
}
