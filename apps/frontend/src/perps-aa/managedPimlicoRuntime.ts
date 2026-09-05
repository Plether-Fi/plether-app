import { createSmartAccountClient } from 'permissionless'
import { SimpleSmartAccount } from 'permissionless/accounts/simple'
import { createPimlicoClient } from 'permissionless/clients/pimlico'
import {
  getAddress,
  hexToBigInt,
  http,
  isAddressEqual,
  isHex,
  maxUint256,
  parseAbi,
  parseAbiItem,
  size,
  type Account,
  type Address,
  type Chain,
  type Hex,
  type PublicClient,
  type Transport,
  type WalletClient,
} from 'viem'
import {
  createBundlerClient,
  createPaymasterClient,
  getUserOperationHash,
  type PaymasterActions,
  type UserOperationReceipt,
} from 'viem/account-abstraction'
import { arbitrumSepolia } from 'viem/chains'
import {
  bundlerRpcUrlForManifest,
  isPerpsAaManifestV2 as isNativePaymasterManifest,
  paymasterRpcUrlForManifest,
  type PerpsAaDeploymentManifest,
} from './manifest'
import { knownSponsorshipValidUntil } from './paymasterValidity'
import type {
  ManagedUserOperation,
  PerpsAaSmartAccountRuntime,
  BundlerUserOperationStatus,
  BundlerUserOperationStatusResult,
  SponsoredOperationRecoverySnapshot,
} from './runtimeContext'
import { UserOperationReceiptNotSafeError } from './runtimeContext'

const BUNDLER_STATUSES = new Set<BundlerUserOperationStatus>([
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
interface CreateManagedAaRuntimeInput {
  manifest: PerpsAaDeploymentManifest
  ownerAddress: Address
  walletClient: WalletClient
  publicClient: PublicClient
}

function parseStatus(value: unknown): BundlerUserOperationStatusResult {
  if (!value || typeof value !== 'object') {
    throw new Error('The bundler returned an invalid UserOperation status')
  }
  const result = value as {
    status?: unknown
    transactionHash?: unknown
  }
  if (
    typeof result.status !== 'string' ||
    !BUNDLER_STATUSES.has(result.status as BundlerUserOperationStatus)
  ) {
    throw new Error('The bundler returned an unknown UserOperation status')
  }
  if (
    result.transactionHash !== null &&
    (
      typeof result.transactionHash !== 'string' ||
      !isHex(result.transactionHash, { strict: true }) ||
      size(result.transactionHash) !== 32
    )
  ) {
    throw new Error('The bundler returned an invalid transaction hash')
  }
  return {
    status: result.status as BundlerUserOperationStatus,
    transactionHash: result.transactionHash,
  }
}

function rpcQuantity(value: unknown, field: string): bigint {
  if (typeof value === 'bigint' && value >= 0n && value <= maxUint256) {
    return value
  }
  if (
    typeof value === 'string' &&
    /^0x(?:0|[1-9a-f][0-9a-f]*)$/i.test(value)
  ) {
    const parsed = hexToBigInt(value as Hex)
    if (parsed <= maxUint256) return parsed
  }
  throw new Error(`The bundler returned an invalid ${field}`)
}

function parseFastUserOperationGasPrice(value: unknown): {
  maxFeePerGas: bigint
  maxPriorityFeePerGas: bigint
} {
  const result = value && typeof value === 'object'
    ? value as { fast?: unknown }
    : undefined
  const fast = result?.fast && typeof result.fast === 'object'
    ? result.fast as {
        maxFeePerGas?: unknown
        maxPriorityFeePerGas?: unknown
      }
    : undefined
  return {
    maxFeePerGas: rpcQuantity(
      fast?.maxFeePerGas,
      'fast maxFeePerGas'
    ),
    maxPriorityFeePerGas: rpcQuantity(
      fast?.maxPriorityFeePerGas,
      'fast maxPriorityFeePerGas'
    ),
  }
}

function requestAltoExtension(
  client: unknown,
  method: 'pimlico_getUserOperationGasPrice' | 'pimlico_getUserOperationStatus',
  params: readonly unknown[]
): Promise<unknown> {
  return (client as {
    request(request: {
      method: string
      params: readonly unknown[]
    }): Promise<unknown>
  }).request({ method, params })
}

function stripPaymasterRequestCredentials<T>(parameters: T): T {
  const unsigned = {
    ...(parameters as Record<string, unknown>),
  }
  // viem prepares an account stub signature before calling its paymaster
  // hooks. Plether's ERC-7677 endpoint must never receive that owner-backed
  // credential or the stub paymaster payload it is about to replace.
  delete unsigned.signature
  delete unsigned.paymaster
  delete unsigned.paymasterData
  return unsigned as T
}

export function createUnsignedPaymasterActions(
  paymasterClient: {
    getPaymasterStubData: PaymasterActions['getPaymasterStubData']
    getPaymasterData: PaymasterActions['getPaymasterData']
  }
): {
  getPaymasterStubData: PaymasterActions['getPaymasterStubData']
  getPaymasterData: PaymasterActions['getPaymasterData']
} {
  return {
    getPaymasterStubData: (parameters) =>
      paymasterClient.getPaymasterStubData(
        stripPaymasterRequestCredentials(parameters)
      ),
    getPaymasterData: (parameters) =>
      paymasterClient.getPaymasterData(
        stripPaymasterRequestCredentials(parameters)
      ),
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
      'The bundler returned a receipt for a different Trading Account operation'
    )
  }
}

function normalizeReceiptNonce(
  receipt: UserOperationReceipt<'0.8'>
): UserOperationReceipt<'0.8'> {
  // Some bundler JSON-RPC responses leave `nonce` as a quantity string at
  // runtime even though permissionless/viem declares it as bigint.
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
      throw new Error('The bundler returned an invalid UserOperation receipt nonce')
    }
    return {
      ...receipt,
      nonce: normalizedNonce,
    }
  }
  throw new Error('The bundler returned an invalid UserOperation receipt nonce')
}

async function assertCanonicalSafeReceipt(input: {
  publicClient: PublicClient
  receipt: UserOperationReceipt<'0.8'>
  expectedHash: Hex
  expectedSender: Address
  entryPoint: Address
  safeBlockNumber?: bigint
}): Promise<void> {
  const [safeBlockNumber, canonicalTransactionReceipt] = await Promise.all([
    input.safeBlockNumber === undefined
      ? input.publicClient.getBlock({ blockTag: 'safe' }).then((block) => block.number)
      : Promise.resolve(input.safeBlockNumber),
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
      'The bundler returned a noncanonical UserOperation transaction receipt'
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
      'The canonical UserOperation event does not match the bundler receipt'
    )
  }
  if (input.receipt.receipt.blockNumber > safeBlockNumber) {
    throw new UserOperationReceiptNotSafeError(input.receipt)
  }
}

function isReceiptNotFoundError(error: unknown): boolean {
  if (!error || typeof error !== 'object') return false
  const name = 'name' in error ? String(error.name) : ''
  return name === 'UserOperationReceiptNotFoundError'
}

export async function createManagedAaRuntime({
  manifest,
  ownerAddress,
  walletClient,
  publicClient,
}: CreateManagedAaRuntimeInput): Promise<PerpsAaSmartAccountRuntime> {
  if (
    manifest.chainId !== arbitrumSepolia.id
  ) {
    throw new Error('The manifest does not describe the supported account stack')
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
  const bundlerRpcUrl = bundlerRpcUrlForManifest(manifest)
  const paymasterRpcUrl = paymasterRpcUrlForManifest(manifest)
  let prepareUserOperation:
    PerpsAaSmartAccountRuntime['smartAccount']['prepareUserOperation']
  let sendUserOperation:
    PerpsAaSmartAccountRuntime['smartAccount']['sendUserOperation']
  let getUserOperationStatus:
    PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationStatus']
  let getUserOperationReceipt:
    PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationReceipt']

  if (isNativePaymasterManifest(manifest)) {
    const paymasterClient = createPaymasterClient({
      transport: http(paymasterRpcUrl),
    })
    const unsignedPaymasterClient = createUnsignedPaymasterActions(
      paymasterClient
    )
    // This client deliberately has no account. Sending a fully signed
    // operation therefore forwards the exact journalled payload instead of
    // preparing, re-sponsoring, or re-signing it a second time.
    const bundlerClient = createBundlerClient({
      chain: arbitrumSepolia,
      transport: http(bundlerRpcUrl),
    })
    const smartAccountClient = createSmartAccountClient({
      account: smartAccount,
      chain: arbitrumSepolia,
      client: publicClient,
      bundlerTransport: http(bundlerRpcUrl),
      paymaster: unsignedPaymasterClient,
      paymasterContext: {},
      userOperation: {
        estimateFeesPerGas: async () => parseFastUserOperationGasPrice(
          await requestAltoExtension(
            bundlerClient,
            'pimlico_getUserOperationGasPrice',
            []
          )
        ),
      },
    })
    prepareUserOperation = async ({ calls }) =>
      await smartAccountClient.prepareUserOperation({
        account: smartAccount,
        calls: calls.map((call) => ({
          to: call.to,
          value: call.value,
          data: call.data,
        })),
      }) as ManagedUserOperation
    sendUserOperation = (operation) =>
      bundlerClient.sendUserOperation({
        ...operation,
        entryPointAddress: manifest.entryPoint,
      })
    getUserOperationStatus = async (userOperationHash) =>
      parseStatus(await requestAltoExtension(
        bundlerClient,
        'pimlico_getUserOperationStatus',
        [userOperationHash]
      ))
    getUserOperationReceipt = (userOperationHash) =>
      bundlerClient.getUserOperationReceipt({ hash: userOperationHash })
  } else {
    const pimlicoClient = createPimlicoClient({
      chain: arbitrumSepolia,
      entryPoint: {
        address: manifest.entryPoint,
        version: manifest.entryPointVersion,
      },
      transport: http(bundlerRpcUrl),
    })
    const smartAccountClient = createSmartAccountClient({
      account: smartAccount,
      chain: arbitrumSepolia,
      client: publicClient,
      bundlerTransport: http(bundlerRpcUrl),
      paymaster: pimlicoClient,
      paymasterContext: {},
      userOperation: {
        estimateFeesPerGas: async () =>
          (await pimlicoClient.getUserOperationGasPrice()).fast,
      },
    })
    prepareUserOperation = async ({ calls }) =>
      await smartAccountClient.prepareUserOperation({
        account: smartAccount,
        calls: calls.map((call) => ({
          to: call.to,
          value: call.value,
          data: call.data,
        })),
      }) as ManagedUserOperation
    sendUserOperation = (operation) =>
      pimlicoClient.sendUserOperation({
        ...operation,
        entryPointAddress: manifest.entryPoint,
      })
    getUserOperationStatus = async (userOperationHash) =>
      parseStatus(await pimlicoClient.getUserOperationStatus({
        hash: userOperationHash,
      }))
    getUserOperationReceipt = (userOperationHash) =>
      pimlicoClient.getUserOperationReceipt({ hash: userOperationHash })
  }
  const accountAddress = getAddress(smartAccount.address)

  return {
    chainId: manifest.chainId,
    ownerAddress: getAddress(ownerAddress),
    factoryAddress: getAddress(manifest.smartAccountFactory),
    accountVersion: manifest.smartAccountVersion,
    accountIndex: manifest.smartAccountIndex,
    manifestVersion: manifest.version,
    sponsorshipValidUntil: (operation) =>
      knownSponsorshipValidUntil(
        operation,
        isNativePaymasterManifest(manifest)
          ? manifest.paymasterAddress
          : undefined
      ),
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
      const [accountNonce, receiptEvidence] = await Promise.all([
        publicClient.readContract({
          address: manifest.entryPoint,
          abi: ENTRY_POINT_NONCE_ABI,
          functionName: 'getNonce',
          args: [accountAddress, nonceKey],
          blockNumber: block.number,
        }),
        getUserOperationReceipt(userOperationHash)
          .then((receipt: UserOperationReceipt<'0.8'> | null) => receipt === null
            ? { kind: 'not-located' as const }
            : { kind: 'located' as const, receipt })
          .catch((error: unknown) => (
            isReceiptNotFoundError(error)
              ? { kind: 'not-located' as const }
              : { kind: 'inconclusive' as const }
          )),
      ])
      let userOperationEvidence:
        SponsoredOperationRecoverySnapshot['userOperationEvidence'] =
          receiptEvidence.kind === 'inconclusive'
            ? { kind: 'inconclusive' }
            : { kind: 'not-located' }
      if (receiptEvidence.kind === 'located') {
        try {
          const receipt = normalizeReceiptNonce(receiptEvidence.receipt)
          assertReceipt(
            receipt,
            userOperationHash,
            accountAddress,
            manifest.entryPoint,
          )
          await assertCanonicalSafeReceipt({
            publicClient,
            receipt,
            expectedHash: userOperationHash,
            expectedSender: accountAddress,
            entryPoint: manifest.entryPoint,
            safeBlockNumber: block.number,
          })
          userOperationEvidence = {
            kind: 'included',
            success: receipt.success,
            transactionHash: receipt.receipt.transactionHash,
            blockNumber: receipt.receipt.blockNumber,
          }
        } catch (error) {
          userOperationEvidence = error instanceof UserOperationReceiptNotSafeError
            ? { kind: 'not-safe-yet' }
            : { kind: 'inconclusive' }
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

      prepareUserOperation: async ({ calls, action }) => {
        return prepareUserOperation({ calls, action })
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

      sendUserOperation,

      getUserOperationStatus,

      getUserOperationReceipt: async (userOperationHash) => {
        const receipt = normalizeReceiptNonce(
          await getUserOperationReceipt(userOperationHash)
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

/** @deprecated Use createManagedAaRuntime for Pimlico/native provider support. */
export const createManagedPimlicoRuntime = createManagedAaRuntime
