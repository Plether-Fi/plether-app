import { encodeFunctionData, getAddress, isHex, keccak256, parseAbi, type Address, type Hex, type PublicClient } from 'viem'
import type { PerpsActionPlan } from '@plether-fi/perps-aa-client'
import { CFD_CLOSE_PREVIEW_ABI } from '../contracts/abis/CfdSponsoredClosePreview'
import type { PerpsOrderRequestV2 } from '../contracts/perpsOrderV2'
import { buildPlaceOrderV2Action } from './orderActionV2'
import { parsePerpsAaManifest, type PerpsAaDeploymentManifest, type PerpsAaDeploymentManifestV2 } from './manifest'

export interface CloseAssistanceConfig {
  lens: Address
  lensCodeHash: Hex
  paymasterAddress: Address
}

export interface SponsoredCloseFunding {
  amountUsdc: bigint
  depositCarryUsdc: bigint
  commitmentCarryUsdc: bigint
  config: CloseAssistanceConfig
}

const FUNDING_ABI = parseAbi([
  'function mint(address to, uint256 amount)',
  'function approve(address spender, uint256 amount) returns (bool)',
  'function depositMargin(uint256 amount)',
])

export async function loadCloseAssistanceConfig(owner?: Address, signal?: AbortSignal): Promise<CloseAssistanceConfig | undefined> {
  const response = await fetch('/api/perps/v1/aa/close-assistance', { signal, cache: 'no-store' })
  if (!response.ok) throw new Error('Close assistance availability could not be checked. Retry review.')
  const body: unknown = await response.json()
  if (typeof body !== 'object' || body === null || Array.isArray(body)) throw new Error('Invalid close assistance configuration')
  const config = body as Record<string, unknown>
  if (config.enabled === false) return undefined
  if (config.enabled !== true || config.chainId !== 421614 || typeof config.lensCodeHash !== 'string' || !isHex(config.lensCodeHash) || config.lensCodeHash.length !== 66) {
    throw new Error('Invalid close assistance configuration')
  }
  if (typeof config.lens !== 'string' || typeof config.paymasterAddress !== 'string') throw new Error('Invalid close assistance configuration')
  if (!Array.isArray(config.canaryOwners) || !config.canaryOwners.every((value: unknown) => typeof value === 'string')) throw new Error('Invalid close assistance cohort')
  if (config.canaryOwners.length > 0 && (!owner || !config.canaryOwners.some((value: unknown) => typeof value === 'string' && getAddress(value) === getAddress(owner)))) return undefined
  return { lens: getAddress(config.lens), lensCodeHash: config.lensCodeHash, paymasterAddress: getAddress(config.paymasterAddress) }
}

export async function verifyCloseAssistanceLens(client: PublicClient, config: CloseAssistanceConfig, blockNumber: bigint) {
  const code = await client.getCode({ address: config.lens, blockNumber })
  if (!code || keccak256(code).toLowerCase() !== config.lensCodeHash.toLowerCase()) {
    throw new Error('Close assistance lens does not match the reviewed deployment')
  }
}

export function closeAssistanceManifest(manifest: PerpsAaDeploymentManifest, config: CloseAssistanceConfig): PerpsAaDeploymentManifestV2 {
  const fields = { ...manifest } as Record<string, unknown>
  delete fields.pimlicoRpcUrl
  return parsePerpsAaManifest({ ...fields,
    bundlerRpcUrl: '/api/perps/v1/aa/rpc', paymasterRpcUrl: '/api/perps/v1/aa/rpc',
    paymasterAddress: config.paymasterAddress, paymasterVersion: 'plether-verifying-v1',
  }) as PerpsAaDeploymentManifestV2
}

export function buildSponsoredCloseAction(manifest: PerpsAaDeploymentManifest, account: Address, request: PerpsOrderRequestV2, funding: SponsoredCloseFunding): PerpsActionPlan {
  const amount = funding.amountUsdc
  if (!request.isClose || request.marginDelta !== 0n || amount <= 0n || amount > 200_000n) throw new Error('Invalid close assistance')
  const call = (to: Address, data: Hex) => Object.freeze({ to, data, value: 0n })
  return Object.freeze({ kind: 'place-order', account, calls: Object.freeze([
    call(funding.config.lens, encodeFunctionData({ abi: CFD_CLOSE_PREVIEW_ABI, functionName: 'validateSponsoredClose', args: [manifest.cfdEngine, request, amount] })),
    call(manifest.usdc, encodeFunctionData({ abi: FUNDING_ABI, functionName: 'mint', args: [account, amount] })),
    call(manifest.usdc, encodeFunctionData({ abi: FUNDING_ABI, functionName: 'approve', args: [manifest.marginClearinghouse, amount] })),
    call(manifest.marginClearinghouse, encodeFunctionData({ abi: FUNDING_ABI, functionName: 'depositMargin', args: [amount] })),
    ...buildPlaceOrderV2Action({ account, orderRouter: manifest.orderRouter, request }).calls,
  ]) })
}

export const SIMPLE_ACCOUNT_BATCH_ABI = parseAbi([
  'function executeBatch((address target, uint256 value, bytes data)[] calls)',
])
