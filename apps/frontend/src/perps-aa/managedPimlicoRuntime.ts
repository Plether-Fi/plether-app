import { createSmartAccountClient } from 'permissionless'
import { SimpleSmartAccount } from 'permissionless/accounts/simple'
import { createPimlicoClient } from 'permissionless/clients/pimlico'
import {
  getAddress,
  http,
  isAddressEqual,
  isHex,
  size,
  type Account,
  type Address,
  type Chain,
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
} from './runtimeContext'

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
  expectedSender: Address
): void {
  if (
    receipt.userOpHash.toLowerCase() !== expectedHash.toLowerCase() ||
    !isAddressEqual(receipt.sender, expectedSender)
  ) {
    throw new Error(
      'Pimlico returned a receipt for a different Trading Account operation'
    )
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
        const receipt = await pimlicoClient.getUserOperationReceipt({
          hash: userOperationHash,
        })
        assertReceipt(receipt, userOperationHash, accountAddress)
        return receipt
      },
    },
  }
}
