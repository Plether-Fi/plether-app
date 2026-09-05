import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import type { ManagedUserOperation } from '../runtimeContext'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const ENTRY_POINT =
  '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108' as Address
const FACTORY =
  '0x13E9ed32155810FDbd067D4522C492D6f68E5944' as Address
const HASH = `0x${'44'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'55'.repeat(32)}` as Hex
const SAFE_BLOCK_HASH = `0x${'66'.repeat(32)}` as Hex
const INCLUDED_BLOCK_HASH = `0x${'77'.repeat(32)}` as Hex
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
  version: 'perps-aa-arbitrum-sepolia-v2',
  chainId: 421614,
  entryPoint: ENTRY_POINT,
  entryPointVersion: '0.8' as const,
  pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
  smartAccountMode: 'simple' as const,
  smartAccountVersion: 'permissionless-simple-v0.8' as const,
  smartAccountIndex: '0',
  smartAccountFactory: FACTORY,
  usdc: '0xAbEe441b564DC084857468fA244AEE0A444B07DF' as Address,
  usdcSupportsEip3009: false,
  usdcEip712Name: null,
  usdcEip712Version: null,
  marginClearinghouse:
    '0x91c85540A1f64C9AEC2C801fcc927F037d619f17' as Address,
  cfdEngine:
    '0x2CEDc3f0059f0E9C1099bE96974f459E58c428d6' as Address,
  orderRouter:
    '0x2b9790AD11cE5fB1B91aC3415B08cD1Ec7D0cE0B' as Address,
  orderLifecycleBook:
    '0xca57215a3859462eb380ea40969762Ac89D99522' as Address,
  policyEvaluator:
    '0x611b34a98261D60f0aE8584F4Dd1fF09CF663466' as Address,
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
    vi.stubGlobal('fetch', vi.fn())
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
      getUserOperationReceipt: vi.fn(async () => null),
    })
  })

  afterEach(() => {
    vi.unstubAllGlobals()
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
      manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
      smartAccount: {
        accountAddress: ACCOUNT,
        entryPoint: ENTRY_POINT,
      },
    })
  })

  it('proves a reorg only when the canonical block hash changed', async () => {
    const getBlock = vi.fn()
      .mockResolvedValueOnce({ number: 123n, hash: INCLUDED_BLOCK_HASH })
      .mockResolvedValueOnce({ number: 123n, hash: SAFE_BLOCK_HASH })
      .mockRejectedValueOnce(new Error('RPC unavailable'))
    const runtime = await createManagedPimlicoRuntime({
      manifest,
      ownerAddress: OWNER,
      walletClient: {
        chain: { id: 421614 },
        account: { address: OWNER },
      } as never,
      publicClient: {
        chain: { id: 421614 },
        getBlock,
      } as never,
    })
    const inclusion = {
      transactionHash: TRANSACTION_HASH,
      blockNumber: 123n,
      blockHash: INCLUDED_BLOCK_HASH,
    }

    await expect(runtime.verifyObservedInclusion?.(inclusion))
      .resolves.toBe('canonical')
    await expect(runtime.verifyObservedInclusion?.(inclusion))
      .resolves.toBe('reorged')
    await expect(runtime.verifyObservedInclusion?.(inclusion))
      .resolves.toBe('unknown')
    expect(getBlock).toHaveBeenCalledTimes(3)
    expect(getBlock).toHaveBeenCalledWith({ blockNumber: 123n })
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

  it('returns a receipt only after its EntryPoint event is canonical and safe', async () => {
    const pimlicoReceipt = {
      actualGasCost: 1n,
      actualGasUsed: 1n,
      entryPoint: ENTRY_POINT,
      logs: [],
      nonce: 7n,
      sender: ACCOUNT,
      success: true,
      userOpHash: HASH,
      receipt: {
        blockHash: INCLUDED_BLOCK_HASH,
        blockNumber: 554n,
        status: 'success',
        transactionHash: TRANSACTION_HASH,
      },
    }
    mocks.createPimlicoClient.mockReturnValue({
      getUserOperationGasPrice: vi.fn(async () => ({
        fast: { maxFeePerGas: 2n, maxPriorityFeePerGas: 1n },
      })),
      sendUserOperation: vi.fn(async () => HASH),
      getUserOperationStatus: vi.fn(),
      getUserOperationReceipt: vi.fn(async () => pimlicoReceipt),
    })
    const getLogs = vi.fn(async () => [{
      blockHash: INCLUDED_BLOCK_HASH,
      blockNumber: 554n,
      transactionHash: TRANSACTION_HASH,
      args: {
        userOpHash: HASH,
        sender: ACCOUNT,
        nonce: 7n,
        success: true,
      },
    }])
    const runtime = await createManagedPimlicoRuntime({
      manifest,
      ownerAddress: OWNER,
      walletClient: {
        chain: { id: 421614 },
        account: { address: OWNER },
      } as never,
      publicClient: {
        chain: { id: 421614 },
        getBlock: vi.fn(async () => ({
          number: 555n,
          timestamp: 1_000n,
          hash: SAFE_BLOCK_HASH,
        })),
        getTransactionReceipt: vi.fn(async () => ({
          blockHash: INCLUDED_BLOCK_HASH,
          blockNumber: 554n,
          status: 'success',
          transactionHash: TRANSACTION_HASH,
        })),
        getLogs,
      } as never,
    })

    await expect(
      runtime.smartAccount.getUserOperationReceipt(HASH)
    ).resolves.toBe(pimlicoReceipt)
    expect(getLogs).toHaveBeenCalledWith(expect.objectContaining({
      address: ENTRY_POINT,
      fromBlock: 554n,
      toBlock: 554n,
      args: {
        userOpHash: HASH,
        sender: ACCOUNT,
      },
    }))
  })

  it('keeps an otherwise valid receipt pending above the safe head', async () => {
    const pimlicoReceipt = {
      actualGasCost: 1n,
      actualGasUsed: 1n,
      entryPoint: ENTRY_POINT,
      logs: [],
      // Match Pimlico's actual JSON-RPC runtime shape. Its public TypeScript
      // type currently claims this field has already been converted to bigint.
      nonce: '0x7' as never,
      sender: ACCOUNT,
      success: true,
      userOpHash: HASH,
      receipt: {
        blockHash: INCLUDED_BLOCK_HASH,
        blockNumber: 556n,
        status: 'success',
        transactionHash: TRANSACTION_HASH,
      },
    }
    mocks.createPimlicoClient.mockReturnValue({
      getUserOperationGasPrice: vi.fn(async () => ({
        fast: { maxFeePerGas: 2n, maxPriorityFeePerGas: 1n },
      })),
      sendUserOperation: vi.fn(async () => HASH),
      getUserOperationStatus: vi.fn(),
      getUserOperationReceipt: vi.fn(async () => pimlicoReceipt),
    })
    const runtime = await createManagedPimlicoRuntime({
      manifest,
      ownerAddress: OWNER,
      walletClient: {
        chain: { id: 421614 },
        account: { address: OWNER },
      } as never,
      publicClient: {
        chain: { id: 421614 },
        getBlock: vi.fn(async () => ({
          number: 555n,
          timestamp: 1_000n,
          hash: SAFE_BLOCK_HASH,
        })),
        getTransactionReceipt: vi.fn(async () => ({
          blockHash: INCLUDED_BLOCK_HASH,
          blockNumber: 556n,
          status: 'success',
          transactionHash: TRANSACTION_HASH,
        })),
        getLogs: vi.fn(async () => [{
          blockHash: INCLUDED_BLOCK_HASH,
          blockNumber: 556n,
          transactionHash: TRANSACTION_HASH,
          args: {
            userOpHash: HASH,
            sender: ACCOUNT,
            nonce: 7n,
            success: true,
          },
        }]),
      } as never,
    })

    await expect(
      runtime.smartAccount.getUserOperationReceipt(HASH)
    ).rejects.toMatchObject({
      name: 'UserOperationReceiptNotSafeError',
      receipt: {
        ...pimlicoReceipt,
        nonce: 7n,
      },
    })
    expect(pimlicoReceipt.nonce).toBe('0x7')
  })

  it('rejects an out-of-range Pimlico receipt nonce', async () => {
    const getUserOperationReceipt = vi.fn(async () => ({
      actualGasCost: 1n,
      actualGasUsed: 1n,
      entryPoint: ENTRY_POINT,
      logs: [],
      nonce: `0x1${'0'.repeat(64)}` as never,
      sender: ACCOUNT,
      success: true,
      userOpHash: HASH,
      receipt: {
        blockHash: INCLUDED_BLOCK_HASH,
        blockNumber: 554n,
        status: 'success',
        transactionHash: TRANSACTION_HASH,
      },
    }))
    mocks.createPimlicoClient.mockReturnValue({
      getUserOperationGasPrice: vi.fn(async () => ({
        fast: { maxFeePerGas: 2n, maxPriorityFeePerGas: 1n },
      })),
      sendUserOperation: vi.fn(async () => HASH),
      getUserOperationStatus: vi.fn(),
      getUserOperationReceipt,
    })
    const getBlock = vi.fn()
    const runtime = await createManagedPimlicoRuntime({
      manifest,
      ownerAddress: OWNER,
      walletClient: {
        chain: { id: 421614 },
        account: { address: OWNER },
      } as never,
      publicClient: {
        chain: { id: 421614 },
        getBlock,
      } as never,
    })

    await expect(
      runtime.smartAccount.getUserOperationReceipt(HASH)
    ).rejects.toThrow('invalid UserOperation receipt nonce')
    expect(getBlock).not.toHaveBeenCalled()
  })

  it('reads recovery time and nonce from the same safe block', async () => {
    const getBlock = vi.fn(async () => ({
      number: 555n,
      timestamp: 1_000n,
      hash: SAFE_BLOCK_HASH,
    }))
    const readContract = vi.fn(async () => 7n)
    const publicClient = {
      chain: { id: 421614 },
      getBlock,
      readContract,
    }
    const runtime = await createManagedPimlicoRuntime({
      manifest,
      ownerAddress: OWNER,
      walletClient: {
        chain: { id: 421614 },
        account: { address: OWNER },
      } as never,
      publicClient: publicClient as never,
    })

    await expect(runtime.getRecoverySnapshot?.(HASH)).resolves.toEqual({
      blockNumber: 555n,
      blockTimestamp: 1_000n,
      accountNonce: 7n,
      userOperationEvidence: { kind: 'not-located' },
    })
    expect(getBlock).toHaveBeenCalledWith({ blockTag: 'safe' })
    expect(readContract).toHaveBeenCalledWith(expect.objectContaining({
      address: ENTRY_POINT,
      functionName: 'getNonce',
      args: [ACCOUNT, 0n],
      blockNumber: 555n,
    }))
    expect(fetch).not.toHaveBeenCalled()
  })

  it('verifies an Alchemy receipt against bounded canonical-chain evidence', async () => {
    const alchemyReceipt = {
      actualGasCost: 1n,
      actualGasUsed: 1n,
      entryPoint: ENTRY_POINT,
      logs: [],
      nonce: 7n,
      sender: ACCOUNT,
      success: true,
      userOpHash: HASH,
      receipt: {
        blockHash: INCLUDED_BLOCK_HASH,
        blockNumber: 554n,
        status: 'success',
        transactionHash: TRANSACTION_HASH,
      },
    }
    mocks.createPimlicoClient.mockReturnValue({
      getUserOperationGasPrice: vi.fn(async () => ({
        fast: { maxFeePerGas: 2n, maxPriorityFeePerGas: 1n },
      })),
      sendUserOperation: vi.fn(async () => HASH),
      getUserOperationStatus: vi.fn(),
      getUserOperationReceipt: vi.fn(async () => alchemyReceipt),
    })
    const getLogs = vi.fn(async () => [{
      blockHash: INCLUDED_BLOCK_HASH,
      blockNumber: 554n,
      transactionHash: TRANSACTION_HASH,
      args: {
        userOpHash: HASH,
        sender: ACCOUNT,
        nonce: 7n,
        success: true,
      },
    }])
    const publicClient = {
      chain: { id: 421614 },
      getBlock: vi.fn(async () => ({
        number: 555n,
        timestamp: 1_000n,
        hash: SAFE_BLOCK_HASH,
      })),
      readContract: vi.fn(async () => 7n),
      getTransactionReceipt: vi.fn(async () => ({
        blockHash: INCLUDED_BLOCK_HASH,
        blockNumber: 554n,
        status: 'success',
        transactionHash: TRANSACTION_HASH,
      })),
      getLogs,
    }
    const runtime = await createManagedPimlicoRuntime({
      manifest,
      ownerAddress: OWNER,
      walletClient: {
        chain: { id: 421614 },
        account: { address: OWNER },
      } as never,
      publicClient: publicClient as never,
    })

    await expect(runtime.getRecoverySnapshot?.(HASH)).resolves.toEqual({
      blockNumber: 555n,
      blockTimestamp: 1_000n,
      accountNonce: 7n,
      userOperationEvidence: {
        kind: 'included',
        success: true,
        transactionHash: TRANSACTION_HASH,
        blockNumber: 554n,
      },
    })
    expect(getLogs).toHaveBeenCalledWith(expect.objectContaining({
      address: ENTRY_POINT,
      args: {
        userOpHash: HASH,
        sender: ACCOUNT,
      },
      fromBlock: 554n,
      toBlock: 554n,
    }))
    expect(fetch).not.toHaveBeenCalled()
  })

  it('treats an Alchemy receipt failure as inconclusive, not chain absence', async () => {
    mocks.createPimlicoClient.mockReturnValue({
      getUserOperationGasPrice: vi.fn(async () => ({
        fast: { maxFeePerGas: 2n, maxPriorityFeePerGas: 1n },
      })),
      sendUserOperation: vi.fn(async () => HASH),
      getUserOperationStatus: vi.fn(),
      getUserOperationReceipt: vi.fn(async () => {
        throw new Error('Alchemy unavailable')
      }),
    })
    const runtime = await createManagedPimlicoRuntime({
      manifest,
      ownerAddress: OWNER,
      walletClient: {
        chain: { id: 421614 },
        account: { address: OWNER },
      } as never,
      publicClient: {
        chain: { id: 421614 },
        getBlock: vi.fn(async () => ({
          number: 555n,
          timestamp: 1_000n,
          hash: SAFE_BLOCK_HASH,
        })),
        readContract: vi.fn(async () => 7n),
      } as never,
    })

    await expect(runtime.getRecoverySnapshot?.(HASH)).resolves.toEqual({
      blockNumber: 555n,
      blockTimestamp: 1_000n,
      accountNonce: 7n,
      userOperationEvidence: { kind: 'inconclusive' },
    })
    expect(fetch).not.toHaveBeenCalled()
  })
})
