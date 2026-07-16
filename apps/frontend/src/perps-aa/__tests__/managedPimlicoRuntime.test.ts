import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import type { ManagedUserOperation } from '../runtimeContext'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const ENTRY_POINT =
  '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108' as Address
const FACTORY =
  '0x13E9ed32155810FDbd067D4522C492D6f68E5944' as Address
const HASH = `0x${'44'.repeat(32)}` as Hex

const mocks = vi.hoisted(() => ({
  toSimpleSmartAccount: vi.fn(),
  createSmartAccountClient: vi.fn(),
  createPimlicoClient: vi.fn(),
  getUserOperationHash: vi.fn(),
}))

vi.mock('permissionless', () => ({
  createSmartAccountClient: mocks.createSmartAccountClient,
}))

vi.mock('permissionless/accounts/simple', () => ({
  SimpleSmartAccount: {
    toSimpleSmartAccount: mocks.toSimpleSmartAccount,
  },
}))

vi.mock('permissionless/clients/pimlico', () => ({
  createPimlicoClient: mocks.createPimlicoClient,
}))

vi.mock('viem/account-abstraction', async (importOriginal) => ({
  ...await importOriginal<typeof import('viem/account-abstraction')>(),
  getUserOperationHash: mocks.getUserOperationHash,
}))

import { createManagedPimlicoRuntime } from '../managedPimlicoRuntime'

const manifest = {
  version: 'perps-aa-arbitrum-sepolia-v1',
  chainId: 421614,
  entryPoint: ENTRY_POINT,
  entryPointVersion: '0.8' as const,
  pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
  smartAccountMode: 'simple' as const,
  smartAccountVersion: 'permissionless-simple-v0.8' as const,
  smartAccountIndex: '0',
  smartAccountFactory: FACTORY,
  usdc: '0xf1e1B188b87525C51ECe4bae8627ae621D769651' as Address,
  usdcSupportsEip3009: false,
  usdcEip712Name: null,
  usdcEip712Version: null,
  marginClearinghouse:
    '0x731bb0939CE531728459394A277B28Cbff8df049' as Address,
  cfdEngine:
    '0xA1Ebfb8aD9C90367eA30A29592419d447E3f8224' as Address,
  orderRouter:
    '0x4A0a6c028164A1254e10C3e39cc89Af45090069e' as Address,
  userOperationExplorerUrlTemplate:
    'https://example.com/user-operation/{userOperationHash}',
  transactionExplorerUrlTemplate:
    'https://example.com/transaction/{transactionHash}',
  testnetFaucet: null,
  sponsorshipEnabled: true,
}

const operation = {
  sender: ACCOUNT,
  nonce: 0n,
  callData: '0x1234',
  callGasLimit: 1n,
  verificationGasLimit: 2n,
  preVerificationGas: 3n,
  maxFeePerGas: 4n,
  maxPriorityFeePerGas: 5n,
  signature: '0xdeadbeef',
} as ManagedUserOperation

describe('createManagedPimlicoRuntime', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.getUserOperationHash.mockReturnValue(HASH)
    mocks.toSimpleSmartAccount.mockResolvedValue({
      address: ACCOUNT,
      signUserOperation: vi.fn(async () => '0xsigned'),
    })
    mocks.createSmartAccountClient.mockReturnValue({
      prepareUserOperation: vi.fn(async () => operation),
    })
    mocks.createPimlicoClient.mockReturnValue({
      getUserOperationGasPrice: vi.fn(async () => ({
        fast: { maxFeePerGas: 2n, maxPriorityFeePerGas: 1n },
      })),
      sendUserOperation: vi.fn(async () => HASH),
      getUserOperationStatus: vi.fn(async () => ({
        status: 'submitted',
        transactionHash: null,
      })),
      getUserOperationReceipt: vi.fn(),
    })
  })

  it('pins deterministic SimpleAccount v0.8 derivation to index and nonce key zero', async () => {
    const walletClient = {
      chain: { id: 421614 },
      account: { address: OWNER },
    }
    const publicClient = {
      chain: { id: 421614 },
    }

    const runtime = await createManagedPimlicoRuntime({
      manifest,
      ownerAddress: OWNER,
      walletClient: walletClient as never,
      publicClient: publicClient as never,
    })

    expect(mocks.toSimpleSmartAccount).toHaveBeenCalledWith(
      expect.objectContaining({
        client: publicClient,
        owner: walletClient,
        entryPoint: {
          address: ENTRY_POINT,
          version: '0.8',
        },
        factoryAddress: FACTORY,
        index: 0n,
        nonceKey: 0n,
      })
    )
    expect(mocks.createPimlicoClient).toHaveBeenCalledWith(
      expect.objectContaining({
        entryPoint: {
          address: ENTRY_POINT,
          version: '0.8',
        },
      })
    )
    expect(runtime).toMatchObject({
      ownerAddress: OWNER,
      factoryAddress: FACTORY,
      accountVersion: 'permissionless-simple-v0.8',
      accountIndex: '0',
      smartAccount: {
        accountAddress: ACCOUNT,
        entryPoint: ENTRY_POINT,
      },
    })
  })

  it('rejects status values outside Pimlico’s reviewed state machine', async () => {
    const pimlicoClient = {
      getUserOperationGasPrice: vi.fn(async () => ({
        fast: { maxFeePerGas: 2n, maxPriorityFeePerGas: 1n },
      })),
      sendUserOperation: vi.fn(async () => HASH),
      getUserOperationStatus: vi.fn(async () => ({
        status: 'mystery',
        transactionHash: null,
      })),
      getUserOperationReceipt: vi.fn(),
    }
    mocks.createPimlicoClient.mockReturnValue(pimlicoClient)

    const runtime = await createManagedPimlicoRuntime({
      manifest,
      ownerAddress: OWNER,
      walletClient: {
        chain: { id: 421614 },
        account: { address: OWNER },
      } as never,
      publicClient: { chain: { id: 421614 } } as never,
    })

    await expect(
      runtime.smartAccount.getUserOperationStatus(HASH)
    ).rejects.toThrow('unknown UserOperation status')
  })
})
