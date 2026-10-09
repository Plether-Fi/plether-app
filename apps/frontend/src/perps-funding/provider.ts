import { decodeFunctionData, encodeFunctionData, erc20Abi, pad, toHex, zeroAddress, type Address, type Hex } from 'viem'
import { ACROSS_PERIPHERY, ETHEREUM_USDC } from './sources'
import { validateFundingDestinationMessage } from './destinationActions'
import { ACROSS_FUNDING_ABI } from './acrossAbi'
import { assertDestination, hash, sameAddress } from './validation'
import type { FundingDestination, FundingManifest, FundingQuote, FundingSource, SourceTransaction } from './types'

export interface FundingWallet {
  request: (request: { method: string; params?: readonly unknown[] }) => Promise<unknown>
}

function allowed(addresses: Address[], candidate: string): boolean { return addresses.some(a => sameAddress(a, candidate)) }
function requireCondition(ok: boolean): asserts ok {
  if (!ok) throw new Error('The funding transaction does not match the reviewed route. No funds were sent.')
}

/** Decode the bridge's economic destination, not just its outer transaction target. */
export function validateAcrossBridge(tx: SourceTransaction, quote: FundingQuote, destination: FundingDestination, source: FundingSource): void {
  requireCondition(tx.kind === 'bridge' && tx.chainId === 1 && tx.value === '0' && allowed(source.transactionTargets, tx.to))
  const decoded = decodeFunctionData({ abi: ACROSS_FUNDING_ABI, data: tx.data })
  // ABI-canonical calls may carry only Across's documented integrator trailer.
  const canonical = encodeFunctionData({ abi: ACROSS_FUNDING_ABI, ...decoded })
  requireCondition(tx.data.toLowerCase().startsWith(canonical.toLowerCase()))
  const trailer = tx.data.slice(canonical.length).toLowerCase()
  requireCondition(trailer === '' || trailer === '73c0de' || /^1dc0de[0-9a-f]{40}73c0de$/.test(trailer))
  const checkTimes = (quoteTimestamp: number, fillDeadline: number) => { requireCondition(quoteTimestamp > 31_536_000 && quote.expiresAt <= quoteTimestamp + 300 && fillDeadline >= quote.expiresAt + 30); }
  if (decoded.functionName === 'swapAndBridge') {
    const swap = decoded.args[0]
    const deposit = swap.depositData
    requireCondition(sameAddress(tx.to, ACROSS_PERIPHERY) && sameAddress(swap.swapToken, quote.sourceToken) && swap.swapTokenAmount === BigInt(quote.sourceAmount) && swap.submissionFees.amount === 0n && swap.submissionFees.recipient === zeroAddress && (swap.transferType === 0 || swap.transferType === 1) && allowed(source.swapExchanges, swap.exchange) && allowed(source.spokePools, swap.spokePool) && swap.minExpectedInputTokenAmount >= BigInt(quote.minimumAmount) && swap.minExpectedInputTokenAmount <= BigInt(quote.sourceAmount) * 105n / 100n && swap.enableProportionalAdjustment && swap.nonce === 0n && swap.routerCalldata.length >= 10)
    requireCondition(sameAddress(deposit.inputToken, ETHEREUM_USDC) && sameAddress(deposit.depositor, destination.owner) && deposit.outputToken.toLowerCase() === pad(destination.token).toLowerCase() && deposit.destinationChainId === BigInt(destination.destinationChainId) && deposit.outputAmount === BigInt(quote.minimumAmount))
    checkTimes(deposit.quoteTimestamp, deposit.fillDeadline)
    requireCondition(deposit.recipient.toLowerCase() === pad(destination.multicallHandler).toLowerCase())
    validateFundingDestinationMessage(deposit.message, quote, destination)
  } else {
    const [depositor, recipient, inputToken, outputToken, inputAmount, outputAmount, destinationChainId, , quoteTimestamp, fillDeadline, , message] = decoded.args
    const match = decoded.functionName === 'deposit' ? (a: string, b: string) => a.toLowerCase() === pad(b as Address).toLowerCase() : sameAddress
    requireCondition(allowed(source.spokePools, tx.to) && match(depositor, destination.owner) && match(recipient, destination.multicallHandler) && match(inputToken, source.token) && match(outputToken, destination.token) && inputAmount === BigInt(quote.sourceAmount) && outputAmount === BigInt(quote.minimumAmount) && destinationChainId === BigInt(destination.destinationChainId) && sameAddress(source.token, ETHEREUM_USDC))
    checkTimes(quoteTimestamp, fillDeadline)
    validateFundingDestinationMessage(message, quote, destination)
  }
}

/** Provider approval payloads can request infinite allowance; replace that with the reviewed exact amount. */
export function prepareFundingTransactions(quote: FundingQuote, destination: FundingDestination, manifest: FundingManifest, now = Date.now()): SourceTransaction[] {
  assertDestination(quote, destination)
  requireCondition(quote.provider === 'across' && manifest.provider === 'across' && quote.sourceChainId === 1 && destination.destinationChainId === 42161 && (quote.expiresAt * 1000) > now && BigInt(quote.sourceAmount) > 0n && BigInt(quote.minimumAmount) > 0n)
  const source = manifest.sources.find(s => s.chainId === quote.sourceChainId && sameAddress(s.token, quote.sourceToken))
  requireCondition(source !== undefined)
  const transactions = quote.sourceTransactions
  requireCondition(transactions.length >= 1 && transactions.length <= 3 && transactions[transactions.length - 1].kind === 'bridge' && transactions.filter(t => t.kind === 'bridge').length === 1)
  return transactions.map(tx => {
    requireCondition(tx.chainId === source.chainId && tx.value === '0')
    if (tx.kind === 'bridge') {
      validateAcrossBridge(tx, quote, destination, source)
      return tx
    }
    requireCondition(sameAddress(tx.to, source.token))
    const approval = decodeFunctionData({ abi: erc20Abi, data: tx.data })
    requireCondition(approval.functionName === 'approve')
    const [spender, approvalAmount] = approval.args
    requireCondition(allowed(source.approvalSpenders, spender) && sameAddress(spender, transactions[transactions.length - 1].to))
    return { ...tx, data: encodeFunctionData({ abi: erc20Abi, functionName: 'approve', args: [spender, approvalAmount === 0n ? 0n : BigInt(quote.sourceAmount)] }) }
  })
}

async function assertWallet(wallet: FundingWallet, owner: Address, expectedChainId: number): Promise<void> {
  const [accounts, activeChain] = await Promise.all([wallet.request({ method: 'eth_accounts' }), wallet.request({ method: 'eth_chainId' })])
  if (!Array.isArray(accounts) || typeof accounts[0] !== 'string' || !sameAddress(accounts[0], owner)) throw new Error('Reconnect the wallet that started this funding intent. Its destination has not changed.')
  if (activeChain !== toHex(expectedChainId)) throw new Error('The wallet network changed. Return to the source network to continue.')
}

export async function sendFundingTransactions(input: {
  wallet: FundingWallet
  destination: FundingDestination
  quote: FundingQuote
  manifest: FundingManifest
  beforeBridge: () => void
  onBridgeHash: (hash: Hex) => void
  onStep: (step: string) => void
}): Promise<Hex> {
  const transactions = prepareFundingTransactions(input.quote, input.destination, input.manifest)
  const wallet = input.wallet
  // Account checks occur both before switching and immediately before every signature.
  const accounts = await wallet.request({ method: 'eth_accounts' })
  if (!Array.isArray(accounts) || typeof accounts[0] !== 'string' || !sameAddress(accounts[0], input.destination.owner)) throw new Error('Connect the original funding wallet.')
  await wallet.request({ method: 'wallet_switchEthereumChain', params: [{ chainId: toHex(input.quote.sourceChainId) }] })
  for (const transaction of transactions) {
    await assertWallet(wallet, input.destination.owner, transaction.chainId)
    if (Date.now() >= (input.quote.expiresAt * 1000)) throw new Error('The funding quote expired. Refresh it before sending funds.')
    input.onStep(transaction.kind === 'approval' ? 'Approve the source token in your wallet' : 'Confirm the transfer in your wallet')
    if (transaction.kind === 'bridge') input.beforeBridge()
    const txHash = hash(await wallet.request({ method: 'eth_sendTransaction', params: [{ from: input.destination.owner, to: transaction.to, data: transaction.data, value: toHex(BigInt(transaction.value)) }] }))
    if (transaction.kind === 'bridge') {
      input.onBridgeHash(txHash)
      return txHash
    }
    input.onStep('Waiting for source-token approval')
    const deadline = Date.now() + 120_000
    let approved = false
    while (Date.now() < deadline) {
      await assertWallet(wallet, input.destination.owner, transaction.chainId)
      const receipt = await wallet.request({ method: 'eth_getTransactionReceipt', params: [txHash] })
      if (receipt && typeof receipt === 'object' && 'status' in receipt) {
        if (receipt.status !== '0x1') throw new Error('The source approval failed. Funds have not been bridged.')
        approved = true
        break
      }
      await new Promise(resolve => setTimeout(resolve, 2_000))
    }
    if (!approved) throw new Error('The approval is still pending. Check your source wallet before continuing.')
  }
  throw new Error('The quote did not contain a bridge transaction.')
}
