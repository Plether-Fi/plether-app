import {
  decodeEventLog,
  getAddress,
  keccak256,
  slice,
  toHex,
  toBytes,
  zeroAddress,
  type Address,
  type Hex,
} from 'viem'
import {
  entryPoint09Abi,
  entryPoint09Address,
  formatUserOperationReceipt,
  formatUserOperationRequest,
  getUserOperationHash,
  type RpcUserOperationReceipt,
  type UserOperation,
  type UserOperationReceipt,
} from 'viem/account-abstraction'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_RELEASE } from '../contracts/perpsAddresses'
import { observeDeadlineResponse } from './deadlineClock'
import { encodeKernelCalls, type KernelAccountPins } from './kernelAccount'
import { kernelSepoliaCanaryPaymaster, kernelSepoliaCanaryProfile } from './kernelDeploymentPins'

export const PERPS_KERNEL_RPC_PATH = '/api/perps/v1/aa/kernel/rpc'
const chainId = 421614
const entryPoint = entryPoint09Address.toLowerCase() as Address
const operationTopic = keccak256(toBytes('UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)'))
const executionTopic = keccak256(toBytes('BeforeExecution()'))
const recoveryTokenPattern = /^v1\.(0x[0-9a-f]{64})\.0x[0-9a-f]{64}\.\d{1,12}\.[0-9a-f]{64}$/
const maxKernelPreVerificationGas = 12_000_000n
const maxKernelAggregateGas = 17_000_000n
const maxKernelEthPrefundWei = 50_000_000_000_000_000n

export type KernelGasMode = 'sponsored' | 'eth'

export interface KernelDeploymentProfile extends KernelAccountPins {
  deployment: string
  chainId: typeof chainId
  entryPointVersion: '0.9'
  accountVersion: 'kernel-v4-uups'
  accountIndex: 0
}

const tradingAddresses = ['settlementToken', 'marginClearinghouse', 'orderRouter', 'cfdEngine', 'positionProtectionBook'] as const
type TradingAddresses = Record<typeof tradingAddresses[number], Address>

export interface KernelManifest extends TradingAddresses {
  version: 1
  profile: KernelDeploymentProfile
  perpsRelease: string
  protectionCommitsEnabled: boolean
  paymaster: Address | null
  readiness: { eth: boolean; sponsored: boolean }
}

export interface KernelPreparationIntent {
  mode: KernelGasMode
  preparationId: Hex
  sender: Address
  callData: Hex
  /** Read directly from EntryPoint in the connected browser. Never sent to the gateway. */
  expectedNonce: bigint
  factory?: Address | undefined
  factoryData?: Hex | undefined
}

export interface KernelPreparedOperation {
  transport?: never
  version: 1
  preparationId: Hex
  mode: KernelGasMode
  entryPoint: Address
  operation: UserOperation<'0.9'>
  userOperationHash: Hex
  expiresAt: number
  maxNetworkGasWei: bigint
  recoveryToken: string
}

export interface KernelWalletPreparation extends Omit<KernelPreparedOperation, 'transport' | 'recoveryToken' | 'mode'> {
  transport: 'wallet'
  mode: 'eth'
}

export type KernelActionPreparation = KernelPreparedOperation | KernelWalletPreparation

export interface KernelPreparedCancellation extends KernelPreparedOperation {
  mode: 'eth'
  cancelsUserOperationHash: Hex
}

export interface KernelStatusLocator {
  preparationId: Hex
  userOperationHash: Hex
  recoveryToken?: string | undefined
  sender: Address
  entryPoint: Address
  nonce: bigint
  paymaster: Address
  cancellationHash?: Hex | undefined
}

export type KernelOperationStatus =
  | { state: 'pending'; receipt: null }
  | { state: 'nonce_consumed'; receipt: null }
  | { state: 'sponsorship_expired'; receipt: null }
  | { state: 'observed'; receipt: UserOperationReceipt<'0.9'> }
  | { state: 'included'; receipt: UserOperationReceipt<'0.9'> }
  | { state: 'cancelled'; receipt: UserOperationReceipt<'0.9'> }

export interface KernelGasFundingAdvice {
  balanceWei: bigint
  entryPointDepositWei: bigint
  maxNetworkGasWei: bigint
  callValueWei: bigint
  shortfallWei: bigint
}

export class KernelRpcError extends Error {
  readonly reason: string
  readonly retryable: boolean
  readonly funding?: KernelGasFundingAdvice

  constructor(reason: string, message: string, retryable = false, funding?: KernelGasFundingAdvice) {
    super(message)
    this.name = 'KernelRpcError'
    this.reason = reason
    this.retryable = retryable
    if (funding) this.funding = funding
  }
}

export function findKernelGasFundingError(error: unknown): KernelRpcError | undefined {
  for (let depth = 0; depth < 8 && error instanceof Error; depth += 1) {
    if (error instanceof KernelRpcError && error.reason === 'INSUFFICIENT_GAS_ETH') return error
    error = error.cause
  }
  return undefined
}

function invalidResponse(): never {
  throw new KernelRpcError('INVALID_RESPONSE', 'The Trading Account service returned an invalid response')
}

function record(value: unknown, fields?: readonly string[]): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return invalidResponse()
  const result = value as Record<string, unknown>
  if (fields && Object.keys(result).some(key => !fields.includes(key))) return invalidResponse()
  return result
}

function bytes(value: unknown, min: number, max = min): Hex {
  if (typeof value !== 'string' || value.length < 2 + min * 2 || value.length > 2 + max * 2 ||
    !/^0x(?:[0-9a-fA-F]{2})*$/.test(value)) return invalidResponse()
  return value.toLowerCase() as Hex
}

function address(value: unknown, allowZero = false): Address {
  const parsed = bytes(value, 20)
  if (!allowZero && parsed === zeroAddress) return invalidResponse()
  return parsed
}

function quantity(value: unknown, min = 0n, max = (1n << 256n) - 1n): bigint {
  if (typeof value !== 'string' || value.length > 66 || !/^0x(?:0|[1-9a-f][0-9a-f]*)$/.test(value)) return invalidResponse()
  const parsed = BigInt(value)
  if (parsed < min || parsed > max) return invalidResponse()
  return parsed
}

function parseFundingAdvice(value: unknown): KernelGasFundingAdvice {
  const raw = record(value, ['balanceWei', 'entryPointDepositWei', 'maxNetworkGasWei', 'callValueWei', 'shortfallWei'])
  const balanceWei = quantity(raw.balanceWei)
  const entryPointDepositWei = quantity(raw.entryPointDepositWei)
  const maxNetworkGasWei = quantity(raw.maxNetworkGasWei, 1n, maxKernelEthPrefundWei)
  const callValueWei = quantity(raw.callValueWei)
  const shortfallWei = quantity(raw.shortfallWei, 1n)
  const liquidGas = maxNetworkGasWei > entryPointDepositWei ? maxNetworkGasWei - entryPointDepositWei : 0n
  if (shortfallWei !== liquidGas + callValueWei - balanceWei) return invalidResponse()
  return { balanceWei, entryPointDepositWei, maxNetworkGasWei, callValueWei, shortfallWei }
}

function seconds(value: unknown): number {
  if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < 1 || value >= 2 ** 48) return invalidResponse()
  return value
}

function recoveryToken(value: unknown, preparationId: Hex): string {
  if (typeof value !== 'string' || recoveryTokenPattern.exec(value)?.[1] !== preparationId) return invalidResponse()
  return value
}

export function parseKernelManifest(value: unknown): KernelManifest {
  const raw = record(value, ['version', 'profile', 'perpsRelease', ...tradingAddresses,
    'protectionCommitsEnabled', 'paymaster', 'readiness'])
  const pins = Object.entries(kernelSepoliaCanaryProfile)
  const profile = record(raw.profile, pins.map(([key]) => key))
  const readiness = record(raw.readiness, ['eth', 'sponsored'])
  const expectedRelease = PERPS_RELEASE.release.version
  const paymaster = raw.paymaster === null ? null : address(raw.paymaster)
  const pinnedPaymaster = kernelSepoliaCanaryPaymaster?.address.toLowerCase()
  const paymasterMatches = paymaster !== null && paymaster === pinnedPaymaster
  if (raw.version !== 1 || !PERPS_RELEASE.isDeployed || !expectedRelease || raw.perpsRelease !== expectedRelease ||
    !tradingAddresses.every(key => address(raw[key]) === PERPS_ARBITRUM_SEPOLIA[key].toLowerCase()) ||
    !pins.every(([key, expected]) => typeof expected === 'string' && expected.startsWith('0x')
      ? bytes(profile[key], (expected.length - 2) / 2) === expected
      : profile[key] === expected) ||
    typeof raw.protectionCommitsEnabled !== 'boolean' || typeof readiness.eth !== 'boolean' || typeof readiness.sponsored !== 'boolean' ||
    (readiness.sponsored && !paymasterMatches) ||
    (pinnedPaymaster !== undefined && paymaster !== null && !paymasterMatches)) return invalidResponse()
  return {
    version: 1,
    profile: kernelSepoliaCanaryProfile,
    perpsRelease: expectedRelease,
    settlementToken: PERPS_ARBITRUM_SEPOLIA.settlementToken,
    marginClearinghouse: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
    orderRouter: PERPS_ARBITRUM_SEPOLIA.orderRouter,
    cfdEngine: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
    positionProtectionBook: PERPS_ARBITRUM_SEPOLIA.positionProtectionBook,
    protectionCommitsEnabled: raw.protectionCommitsEnabled,
    paymaster: paymasterMatches ? paymaster : null,
    readiness: { eth: readiness.eth, sponsored: readiness.sponsored },
  }
}

const operationKeys = ['sender', 'nonce', 'callData', 'callGasLimit', 'verificationGasLimit', 'preVerificationGas',
  'maxFeePerGas', 'maxPriorityFeePerGas', 'factory', 'factoryData'] as const
const paymasterKeys = ['paymaster', 'paymasterData', 'paymasterVerificationGasLimit', 'paymasterPostOpGasLimit'] as const

function parseKernelOperation(value: unknown, mode: KernelGasMode): UserOperation<'0.9'> {
  const raw = record(value, mode === 'eth' ? operationKeys : [...operationKeys, ...paymasterKeys])
  const factory = raw.factory === undefined ? undefined : address(raw.factory)
  const factoryData = raw.factoryData === undefined ? undefined : bytes(raw.factoryData, 1, 2048)
  if ((factory === undefined) !== (factoryData === undefined)) return invalidResponse()
  const base = {
    sender: address(raw.sender),
    nonce: quantity(raw.nonce, 0n, (1n << 64n) - 1n),
    callData: bytes(raw.callData, 1, 16_384),
    callGasLimit: quantity(raw.callGasLimit, 1n, 3_000_000n),
    verificationGasLimit: quantity(raw.verificationGasLimit, 1n, 1_000_000n),
    preVerificationGas: quantity(raw.preVerificationGas, 1n, maxKernelPreVerificationGas),
    maxFeePerGas: quantity(raw.maxFeePerGas, 1n, 10_000_000_000n),
    maxPriorityFeePerGas: quantity(raw.maxPriorityFeePerGas, 0n, 2_000_000_000n),
    signature: '0x' as const,
    ...(factory && factoryData ? { factory, factoryData } : {}),
  }
  const baseGas = base.callGasLimit + base.verificationGasLimit + base.preVerificationGas
  if (base.maxPriorityFeePerGas > base.maxFeePerGas || baseGas > maxKernelAggregateGas) {
    return invalidResponse()
  }
  if (mode === 'eth') {
    if (baseGas * base.maxFeePerGas > maxKernelEthPrefundWei) return invalidResponse()
    return base
  }
  const paymasterData = bytes(raw.paymasterData, 157)
  const paymasterVerificationGasLimit = quantity(raw.paymasterVerificationGasLimit, 1n, 1_000_000n)
  const paymasterPostOpGasLimit = quantity(raw.paymasterPostOpGasLimit, 0n, 0n)
  const gas = baseGas + paymasterVerificationGasLimit
  const validUntil = BigInt(slice(paymasterData, 0, 6))
  const validAfter = BigInt(slice(paymasterData, 6, 12))
  const ceiling = BigInt(slice(paymasterData, 12, 28))
  if (paymasterData.endsWith('22e325a297439656') || gas > maxKernelAggregateGas || validUntil <= validAfter ||
    validUntil - validAfter > 600n || ceiling < gas * base.maxFeePerGas || ceiling > 10_000_000_000_000_000n) {
    return invalidResponse()
  }
  return { ...base, paymaster: address(raw.paymaster), paymasterData, paymasterVerificationGasLimit, paymasterPostOpGasLimit }
}

export function getKernelOperationHash(operation: UserOperation<'0.9'>): Hex {
  return getUserOperationHash({ userOperation: operation, chainId, entryPointAddress: entryPoint, entryPointVersion: '0.9' })
}

function maxNetworkGas(operation: UserOperation<'0.9'>): bigint {
  return (operation.callGasLimit + operation.verificationGasLimit + operation.preVerificationGas +
    (operation.paymasterVerificationGasLimit ?? 0n) + (operation.paymasterPostOpGasLimit ?? 0n)) * operation.maxFeePerGas
}

function validateSponsorshipExpiry(operation: UserOperation<'0.9'>, expiresAt: number) {
  if (operation.paymasterData && BigInt(expiresAt + 30) !== BigInt(slice(operation.paymasterData, 0, 6))) return invalidResponse()
}

export function assertKernelPreparedOperation(value: unknown): asserts value is KernelPreparedOperation {
  const prepared = record(value)
  const operation = record(prepared.operation, [...operationKeys, ...paymasterKeys, 'signature'])
  if (prepared.version !== 1 || prepared.entryPoint !== entryPoint || operation.signature !== '0x' ||
    (prepared.mode !== 'eth' && prepared.mode !== 'sponsored')) return invalidResponse()
  const wire = { ...operation }
  delete wire.signature
  for (const key of ['nonce', 'callGasLimit', 'verificationGasLimit', 'preVerificationGas', 'maxFeePerGas', 'maxPriorityFeePerGas',
    ...(prepared.mode === 'sponsored' ? ['paymasterVerificationGasLimit', 'paymasterPostOpGasLimit'] : [])]) {
    const number = wire[key]
    if (typeof number !== 'bigint' || number < 0n) return invalidResponse()
    wire[key] = toHex(number)
  }
  const parsedId = bytes(prepared.preparationId, 32)
  const parsed = parseKernelOperation(wire, prepared.mode)
  if (prepared.preparationId !== parsedId || getKernelOperationHash(parsed) !== prepared.userOperationHash ||
    prepared.maxNetworkGasWei !== maxNetworkGas(parsed)) {
    return invalidResponse()
  }
  validateSponsorshipExpiry(parsed, seconds(prepared.expiresAt))
  recoveryToken(prepared.recoveryToken, parsedId)
}

interface CancellationBinding {
  original: KernelPreparedOperation
  hash: Hex
}

export function parseKernelPreparation(
  value: unknown, manifest: KernelManifest, intent: KernelPreparationIntent, now: number, cancellation?: CancellationBinding,
): KernelPreparedOperation {
  const raw = record(value, ['version', 'preparationId', 'mode', 'entryPoint', 'operation', 'expiresAt',
    'maxNetworkGasWei', 'recoveryToken', ...(cancellation ? ['cancelsUserOperationHash'] : [])])
  const preparationId = bytes(raw.preparationId, 32)
  if (raw.version !== 1 || raw.mode !== intent.mode || preparationId !== intent.preparationId.toLowerCase() ||
    address(raw.entryPoint) !== entryPoint || (cancellation && bytes(raw.cancelsUserOperationHash, 32) !== cancellation.hash)) {
    return invalidResponse()
  }
  const operation = parseKernelOperation(raw.operation, intent.mode)
  if (operation.nonce !== intent.expectedNonce || operation.sender !== intent.sender.toLowerCase() || operation.callData !== intent.callData.toLowerCase() ||
    operation.factory !== intent.factory?.toLowerCase() || operation.factoryData !== intent.factoryData?.toLowerCase() ||
    (operation.factory && operation.factory !== manifest.profile.factory)) return invalidResponse()
  const expiresAt = seconds(raw.expiresAt)
  if (!Number.isFinite(now) || expiresAt <= now || expiresAt > now + (intent.mode === 'sponsored' ? 570 : 120)) {
    return invalidResponse()
  }
  const gas = quantity(raw.maxNetworkGasWei, 1n)
  if (gas !== maxNetworkGas(operation)) return invalidResponse()
  if (intent.mode === 'sponsored') {
    const data = operation.paymasterData
    if (!data || operation.paymaster !== manifest.paymaster || slice(data, 60, 92) !== manifest.profile.accountCodeHash ||
      BigInt(slice(data, 6, 12)) >= BigInt(Math.ceil(now))) return invalidResponse()
    validateSponsorshipExpiry(operation, expiresAt)
  }
  if (cancellation) {
    const original = cancellation.original.operation
    const bump = (fee: bigint) => fee + (fee === 0n ? 1n : (fee + 9n) / 10n)
    if (operation.nonce !== original.nonce || operation.maxFeePerGas < bump(original.maxFeePerGas) ||
      operation.maxPriorityFeePerGas < bump(original.maxPriorityFeePerGas)) return invalidResponse()
  }
  return {
    version: 1, preparationId, mode: intent.mode, entryPoint, operation, expiresAt,
    userOperationHash: getKernelOperationHash(operation), maxNetworkGasWei: gas,
    recoveryToken: recoveryToken(raw.recoveryToken, preparationId),
  }
}

function logLocator(value: unknown, transactionHash: Hex, blockHash: Hex, blockNumber: bigint) {
  const log = record(value)
  if (bytes(log.transactionHash, 32) !== transactionHash || bytes(log.blockHash, 32) !== blockHash ||
    quantity(log.blockNumber) !== blockNumber || (log.removed !== undefined && log.removed !== false) ||
    !Array.isArray(log.topics) || log.topics.length > 4) return invalidResponse()
  return {
    address: address(log.address, true),
    topics: log.topics.map((topic: unknown) => bytes(topic, 32)),
    data: bytes(log.data, 0, 1_048_576),
    index: quantity(log.logIndex, 0n, BigInt(Number.MAX_SAFE_INTEGER)),
  }
}

function parseKernelReceipt(value: unknown, locator: KernelStatusLocator): UserOperationReceipt<'0.9'> {
  const raw = record(value, ['userOpHash', 'sender', 'entryPoint', 'nonce', 'paymaster', 'actualGasCost',
    'actualGasUsed', 'success', 'logs', 'receipt'])
  const nested = record(raw.receipt)
  const userOpHash = bytes(raw.userOpHash, 32)
  const sender = address(raw.sender)
  const paymaster = address(raw.paymaster, true)
  const nonce = quantity(raw.nonce)
  const actualGasCost = quantity(raw.actualGasCost)
  const actualGasUsed = quantity(raw.actualGasUsed)
  const blockHash = bytes(nested.blockHash, 32)
  const blockNumber = quantity(nested.blockNumber)
  const transactionHash = bytes(nested.transactionHash, 32)
  if (userOpHash !== locator.userOperationHash.toLowerCase() || sender !== locator.sender.toLowerCase() ||
    address(raw.entryPoint) !== address(locator.entryPoint) || address(raw.entryPoint) !== entryPoint ||
    nonce !== locator.nonce || paymaster !== locator.paymaster.toLowerCase() ||
    typeof raw.success !== 'boolean' || nested.status !== '0x1' || !Array.isArray(raw.logs) ||
    !Array.isArray(nested.logs) || !raw.logs.length || !nested.logs.length) return invalidResponse()
  const logs = nested.logs.map((log: unknown) => logLocator(log, transactionHash, blockHash, blockNumber))
  if (logs.some((log, index) => index > 0 && log.index <= logs[index - 1].index)) return invalidResponse()
  const matches = logs.filter(log => log.address === entryPoint && log.topics[0] === operationTopic && log.topics[1] === userOpHash)
  if (matches.length !== 1 || matches[0].topics.length !== 4) return invalidResponse()
  const match = matches[0]
  const [signatureTopic, hashTopic, senderTopic, paymasterTopic] = match.topics
  const { args } = decodeEventLog({
    abi: entryPoint09Abi, eventName: 'UserOperationEvent',
    topics: [signatureTopic, hashTopic, senderTopic, paymasterTopic], data: match.data,
  })
  if (args.sender.toLowerCase() !== sender || args.paymaster.toLowerCase() !== paymaster || args.nonce !== nonce ||
    args.success !== raw.success || args.actualGasCost !== actualGasCost || args.actualGasUsed !== actualGasUsed) {
    return invalidResponse()
  }
  const boundary = logs.filter(log => log.index < match.index && log.address === entryPoint &&
    (log.topics[0] === operationTopic || log.topics[0] === executionTopic)).at(-1)
  if (!boundary) return invalidResponse()
  const bounded = logs.filter(log => log.index > boundary.index && log.index <= match.index)
  const scoped = raw.logs.map((log: unknown) => logLocator(log, transactionHash, blockHash, blockNumber))
  if (bounded.length !== scoped.length || bounded.some((log, index) => {
    const other = scoped[index]
    return log.index !== other.index || log.address !== other.address || log.data !== other.data ||
      log.topics.join(',') !== other.topics.join(',')
  })) return invalidResponse()
  const formatted = formatUserOperationReceipt(raw as unknown as RpcUserOperationReceipt)
  return { ...formatted, nonce, sender: getAddress(sender), entryPoint, paymaster: getAddress(paymaster), userOpHash }
}

export function parseKernelOperationStatus(value: unknown, locator: KernelStatusLocator): KernelOperationStatus {
  const raw = record(value, ['state', 'receipt'])
  switch (raw.state) {
    case 'pending':
    case 'nonce_consumed':
    case 'sponsorship_expired':
      if (raw.receipt !== null || (raw.state === 'sponsorship_expired' && address(locator.paymaster, true) === zeroAddress)) {
        return invalidResponse()
      }
      return { state: raw.state, receipt: null }
    case 'observed':
    case 'included':
      return { state: raw.state, receipt: parseKernelReceipt(raw.receipt, locator) }
    case 'cancelled': {
      if (address(locator.paymaster, true) !== zeroAddress) return invalidResponse()
      const replacement = bytes(record(raw.receipt).userOpHash, 32)
      if (replacement === locator.userOperationHash.toLowerCase()) return invalidResponse()
      const receipt = parseKernelReceipt(raw.receipt, { ...locator, userOperationHash: replacement })
      return locator.cancellationHash?.toLowerCase() === replacement
        ? { state: 'cancelled', receipt }
        : { state: 'nonce_consumed', receipt: null }
    }
    default:
      return invalidResponse()
  }
}

async function rpc(method: string, params: unknown[], signal?: AbortSignal) {
  const id = crypto.randomUUID()
  const startedAt = performance.now()
  const timeout = AbortSignal.timeout(30_000)
  const response = await fetch(PERPS_KERNEL_RPC_PATH, {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, credentials: 'same-origin',
    cache: 'no-store', redirect: 'error', signal: signal ? AbortSignal.any([timeout, signal]) : timeout,
    body: JSON.stringify({ jsonrpc: '2.0', id, method, params }),
  })
  let clock
  try { clock = observeDeadlineResponse(response, startedAt) } catch { /* Read-only status remains usable. */ }
  const json: unknown = await response.json().catch(() => invalidResponse())
  const body = record(json)
  if (body.jsonrpc !== '2.0') return invalidResponse()
  if (body.error !== undefined && body.result === undefined && (body.id === id || body.id === null)) {
    const error = record(body.error)
    const details = record(error.data)
    if (typeof details.reason !== 'string' || !/^[A-Z0-9_]{1,64}$/.test(details.reason) ||
      typeof details.retryable !== 'boolean' || typeof error.message !== 'string' || error.message.length > 300) {
      return invalidResponse()
    }
    const funding = details.funding !== undefined && details.reason === 'INSUFFICIENT_GAS_ETH' && method === 'plether_prepareUserOperation'
      ? parseFundingAdvice(details.funding) : undefined
    throw new KernelRpcError(details.reason, error.message, details.retryable, funding)
  }
  if (!response.ok || body.id !== id || body.result === undefined || body.error !== undefined) return invalidResponse()
  return { result: body.result, now: clock ? clock.now() / 1000 : NaN }
}

export async function fetchKernelManifest(signal?: AbortSignal): Promise<KernelManifest> {
  const { result } = await rpc('plether_getKernelManifest', [], signal)
  return parseKernelManifest(result)
}

export async function prepareKernelOperation(
  manifest: KernelManifest, intent: KernelPreparationIntent, signal?: AbortSignal,
): Promise<KernelPreparedOperation> {
  const { expectedNonce, ...request } = intent
  if (typeof expectedNonce !== 'bigint' || expectedNonce < 0n || expectedNonce >> 64n !== 0n) return invalidResponse()
  const { result, now } = await rpc('plether_prepareUserOperation', [{ version: 1, ...request }], signal)
  return parseKernelPreparation(result, manifest, intent, now)
}

export async function sendKernelOperation(prepared: KernelPreparedOperation, signature: Hex, signal?: AbortSignal): Promise<Hex> {
  assertKernelPreparedOperation(prepared)
  const operation = formatUserOperationRequest({ ...prepared.operation, signature: bytes(signature, 65) })
  const { result } = await rpc('plether_sendUserOperation', [{
    version: 1, preparationId: prepared.preparationId, mode: prepared.mode, operation, recoveryToken: prepared.recoveryToken,
  }], signal)
  const hash = bytes(result, 32)
  if (hash !== prepared.userOperationHash) return invalidResponse()
  return hash
}

export async function getKernelOperationStatus(locator: KernelStatusLocator, signal?: AbortSignal): Promise<KernelOperationStatus> {
  const preparationId = bytes(locator.preparationId, 32)
  const { result } = await rpc('plether_getUserOperationStatus', [{
    version: 1, preparationId, userOperationHash: bytes(locator.userOperationHash, 32),
    recoveryToken: locator.recoveryToken ? recoveryToken(locator.recoveryToken, preparationId) : undefined,
  }], signal)
  return parseKernelOperationStatus(result, locator)
}

function originalCancellationLocator(original: KernelPreparedOperation) {
  assertKernelPreparedOperation(original)
  if (original.mode !== 'eth') return invalidResponse()
  return { originalPreparationId: original.preparationId, originalUserOperationHash: original.userOperationHash }
}

export async function prepareKernelCancellation(
  manifest: KernelManifest,
  original: KernelPreparedOperation,
  binding: Pick<KernelPreparationIntent, 'preparationId' | 'factory' | 'factoryData'> & { originalSignature?: Hex },
  signal?: AbortSignal,
): Promise<KernelPreparedCancellation> {
  const locator = originalCancellationLocator(original)
  const sender = original.operation.sender
  const callData = encodeKernelCalls([{ to: sender }])
  const { originalSignature, ...intent } = binding
  const originalOperation = originalSignature === undefined ? undefined
    : formatUserOperationRequest({ ...original.operation, signature: bytes(originalSignature, 65) })
  const { result, now } = await rpc('plether_prepareUserOperationCancellation', [{
    version: 1, ...locator, preparationId: intent.preparationId, recoveryToken: original.recoveryToken, originalOperation,
  }], signal)
  return {
    ...parseKernelPreparation(result, manifest, { ...intent, mode: 'eth', sender, callData, expectedNonce: original.operation.nonce }, now, {
      original, hash: locator.originalUserOperationHash,
    }),
    mode: 'eth',
    cancelsUserOperationHash: locator.originalUserOperationHash,
  }
}

export async function sendKernelCancellation(
  original: KernelPreparedOperation, prepared: KernelPreparedCancellation, signature: Hex, signal?: AbortSignal,
): Promise<Hex> {
  const locator = originalCancellationLocator(original)
  assertKernelPreparedOperation(prepared)
  if (prepared.cancelsUserOperationHash !== locator.originalUserOperationHash ||
    prepared.operation.nonce !== original.operation.nonce || prepared.operation.sender !== original.operation.sender ||
    prepared.operation.callData.toLowerCase() !== encodeKernelCalls([{ to: original.operation.sender }])) return invalidResponse()
  // The replacement preparation has its own token. Authorization to replace
  // an operation after an IP change remains scoped to the original operation.
  const { result } = await rpc('plether_sendUserOperationCancellation', [{
    version: 1, ...locator, preparationId: prepared.preparationId, recoveryToken: original.recoveryToken,
    operation: formatUserOperationRequest({ ...prepared.operation, signature: bytes(signature, 65) }),
  }], signal)
  const hash = bytes(result, 32)
  if (hash !== prepared.userOperationHash) return invalidResponse()
  return hash
}
