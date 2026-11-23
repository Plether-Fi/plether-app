import { buildDepositForCalls, marginClearinghouseFundingAbi as fundingAbi } from '@plether-fi/perps-aa-client'
import { createStore, del, get, set } from 'idb-keyval'
import { isAddressEqual, keccak256, parseEventLogs, zeroAddress, type PublicClient, type WalletClient } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import { ERC20_ABI } from '../contracts/abis'
import { PERPS_RELEASE } from '../contracts/perpsAddresses'
import { estimateKernelOwnerTransactionFees } from './kernelEthFunding'
import { kernelAttemptScopeKey, withKernelOperationLane } from './kernelOperationJournal'
import { findKernelOwnerTransaction, type KernelWalletSubmission } from './kernelWalletPayment'
import { isExplicitSignatureRejection } from './preparedOperation'
import type { KernelPerpsAaSmartAccountRuntime } from './runtimeContext'

const journal = createStore('plether-perps-owner-deposits', 'pending')
type PendingDeposit = KernelWalletSubmission & { amount: bigint }
type DepositStage = 'approving' | 'depositing'

export async function depositKernelMarginFromOwner({ client, wallet, runtime, amount, requireCurrent, onStage }: {
  client: PublicClient
  wallet: WalletClient
  runtime: KernelPerpsAaSmartAccountRuntime
  amount: bigint
  requireCurrent: () => void
  onStage?: (stage: DepositStage) => void
}) {
  const { ownerAddress, kernel: { scope } } = runtime
  const token = PERPS_RELEASE.contracts.settlementToken
  const house = PERPS_RELEASE.contracts.marginClearinghouse
  if (!runtime.gasPayment.paysEthFromWallet || scope.chainId !== arbitrumSepolia.id || client.chain?.id !== scope.chainId ||
      !token.address || !token.runtimeCodeHash || !house.address || !house.runtimeCodeHash ||
      !wallet.account || !isAddressEqual(wallet.account.address, ownerAddress)) {
    throw new Error('Direct deposits require the reviewed Arbitrum Sepolia Trading Account and Owner Wallet.')
  }
  const settlementToken = token.address
  const clearinghouse = house.address
  const tokenCodeHash = token.runtimeCodeHash.toLowerCase()
  const houseCodeHash = house.runtimeCodeHash.toLowerCase()
  const calls = (depositAmount: bigint) => buildDepositForCalls({
    settlementToken, clearinghouse, beneficiary: scope.sender, amount: depositAmount,
  })
  const key = `${kernelAttemptScopeKey(scope)}:${ownerAddress.toLowerCase()}:${clearinghouse.toLowerCase()}`
  const assertOwner = async () => {
    const [addresses, chainId] = await Promise.all([wallet.getAddresses(), wallet.getChainId()])
    const selected = addresses.at(0)
    requireCurrent()
    if (chainId !== scope.chainId || !selected || !isAddressEqual(selected, ownerAddress)) {
      throw new Error('Reconnect the Owner Wallet on Arbitrum Sepolia before depositing.')
    }
  }
  const readAllowance = () => client.readContract({
    address: settlementToken, abi: ERC20_ABI, functionName: 'allowance', args: [ownerAddress, clearinghouse],
  })

  const settle = async (pending: PendingDeposit) => {
    const expected = calls(pending.amount)[1]
    const receipt = pending.transactionHash
      ? await client.waitForTransactionReceipt({ hash: pending.transactionHash, timeout: 60_000 }).catch(() => undefined)
      : undefined
    const included = receipt ?? await findKernelOwnerTransaction(client, ownerAddress, pending, await client.getBlockNumber({ cacheTime: 0 }))
    if (!included) throw new Error('The Owner Wallet deposit is still unverified. Check your wallet activity before submitting again.')
    const [block, transaction] = await Promise.all([
      client.getBlock({ blockNumber: included.blockNumber }),
      client.getTransaction({ hash: included.transactionHash }),
    ])
    if (block.number !== included.blockNumber || block.hash !== included.blockHash ||
        transaction.blockNumber !== included.blockNumber || transaction.blockHash !== included.blockHash ||
        transaction.hash !== included.transactionHash || !isAddressEqual(transaction.from, ownerAddress)) {
      throw new Error('The deposit receipt could not be verified. Check your Owner Wallet activity.')
    }
    if (!transaction.to || !isAddressEqual(transaction.to, clearinghouse) ||
        transaction.input.toLowerCase() !== expected.data.toLowerCase() || transaction.value !== 0n) {
      if (pending.transactionHash && transaction.nonce === pending.nonce) {
        await del(key, journal)
        throw new Error('The deposit was replaced in your Owner Wallet. Review a fresh deposit.')
      }
      throw new Error('The deposit receipt could not be verified. Check your Owner Wallet activity.')
    }
    if (included.status !== 'success') {
      await del(key, journal)
      throw new Error('The Owner Wallet deposit reverted. Review a fresh deposit.')
    }
    const credited = parseEventLogs({ abi: fundingAbi, eventName: 'DepositFor', logs: included.logs }).some(log =>
      isAddressEqual(log.address, clearinghouse) && isAddressEqual(log.args.payer, ownerAddress) &&
      isAddressEqual(log.args.account, scope.sender) && log.args.amount === pending.amount)
    if (!credited) throw new Error('The Margin Account credit could not be verified. Check your Owner Wallet activity.')
    await del(key, journal)
    if (pending.amount !== amount) throw new Error('Your previous deposit confirmed. Review the updated margin balance before depositing again.')
    return included.transactionHash
  }

  return withKernelOperationLane(scope, async () => {
    requireCurrent()
    const pending = await get<PendingDeposit>(key, journal)
    if (pending) return settle(pending)

    const [approval, deposit] = calls(amount)
    const verified = await runtime.kernel.verifyAccount()
    if (!isAddressEqual(verified.address, scope.sender) || !isAddressEqual(verified.ownerAddress, ownerAddress) ||
        isAddressEqual(scope.sender, zeroAddress)) throw new Error('The Margin Account recipient could not be verified.')
    const { blockNumber, blockHash } = verified
    const [tokenCode, houseCode, asset, balance] = await Promise.all([
      client.getBytecode({ address: settlementToken, blockNumber }),
      client.getBytecode({ address: clearinghouse, blockNumber }),
      client.readContract({ address: clearinghouse, abi: fundingAbi, functionName: 'settlementAsset', blockNumber }),
      client.readContract({ address: settlementToken, abi: ERC20_ABI, functionName: 'balanceOf', args: [ownerAddress], blockNumber }),
    ])
    const block = await client.getBlock({ blockNumber })
    if (!tokenCode || keccak256(tokenCode) !== tokenCodeHash ||
        !houseCode || keccak256(houseCode) !== houseCodeHash ||
        !isAddressEqual(asset, settlementToken) || block.hash !== blockHash) {
      throw new Error('The reviewed Margin Account funding contracts could not be verified.')
    }
    if (balance < amount) throw new Error('The Owner Wallet has insufficient USDG for this deposit.')

    if (await readAllowance() < amount) {
      const fees = await estimateKernelOwnerTransactionFees(client)
      await assertOwner()
      onStage?.('approving')
      const hash = await wallet.sendTransaction({ account: ownerAddress, chain: arbitrumSepolia, ...approval, ...fees })
      const receipt = await client.waitForTransactionReceipt({ hash, timeout: 60_000 })
      if (receipt.status !== 'success' || await readAllowance() < amount) {
        throw new Error('The USDG approval did not confirm for this deposit.')
      }
    }
    await client.simulateContract({ address: clearinghouse, abi: fundingAbi, functionName: 'depositFor',
      args: [scope.sender, amount], account: ownerAddress })
    const [fees, fromBlock, nonce] = await Promise.all([
      estimateKernelOwnerTransactionFees(client), client.getBlockNumber({ cacheTime: 0 }),
      client.getTransactionCount({ address: ownerAddress, blockTag: 'pending' }),
    ])
    await assertOwner()
    const submission = { amount, fromBlock, nonce }
    await set(key, submission, journal)
    onStage?.('depositing')
    let transactionHash
    try {
      transactionHash = await wallet.sendTransaction({ account: ownerAddress, chain: arbitrumSepolia, ...deposit, nonce, ...fees })
    } catch (error) {
      if (isExplicitSignatureRejection(error)) {
        await del(key, journal)
        throw error
      }
      throw new Error('The Owner Wallet deposit could not be verified. Check your wallet activity before submitting again.', { cause: error })
    }
    const submitted = { ...submission, transactionHash }
    await set(key, submitted, journal)
    return settle(submitted)
  }, { mode: 'eth' })
}
