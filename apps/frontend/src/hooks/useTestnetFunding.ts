import { reportedTransactionError } from '../analytics/transactionErrors'
import { useEffect, useRef, useState } from 'react'
import { Result } from 'better-result'
import { isAddressEqual, isHash, type Address } from 'viem'
import { usePublicClient } from 'wagmi'
import { perpsApi, testnetFaucetErrorMessage } from '../api'
import type { LegacyMarginFunding, MarginFundingProgress, TestnetFaucetClaim } from '../api/types'
import { ERC20_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, PERPS_SETTLEMENT_TOKEN, PERPS_SETTLEMENT_TOKEN_LABEL, PERPS_RELEASE_IS_DEPLOYED } from '../contracts/perpsAddresses'
import { usePerpsIdentity } from '../perps-aa'
import { usePerpsTrading } from './usePerpsTrading'

export type TestnetFundingPhase = 'idle' | 'claiming' | 'approving' | 'waiting' | 'depositing' | 'deposited'

export function useTestnetFunding(recipient: string, enabled: boolean) {
  const identity = usePerpsIdentity()
  const client = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const { depositMargin, refreshAccount } = usePerpsTrading()
  const [claim, setClaim] = useState<TestnetFaucetClaim | null>(null)
  const [phase, setPhase] = useState<TestnetFundingPhase>('idle')
  const [error, setError] = useState<string | null>(null)
  const operation = useRef<AbortController | null>(null)

  useEffect(() => () => {
    operation.current?.abort()
    operation.current = null
    setPhase('idle')
    setClaim(null)
    setError(null)
  }, [
    recipient, enabled, identity.ownerAddress, identity.accountAddress,
    identity.chainId, identity.status, identity.isAaManifestConfigured, identity.sponsorshipEnabled,
  ])

  async function requestFunds(address: Address) {
    if (!enabled || operation.current || phase === 'deposited' || !PERPS_RELEASE_IS_DEPLOYED) return
    const controller = new AbortController()
    operation.current = controller
    const { signal } = controller
    const isCancelled = () => signal.aborted
    setError(null)
    setPhase('claiming')
    let supportId: string | undefined
    let failureStage = 'funding'

    try {
      if (!PERPS_SETTLEMENT_TOKEN.isMock || PERPS_SETTLEMENT_TOKEN.symbol !== 'USDG') {
        throw new Error('USDG funding is unavailable. Please try again later.')
      }
      if (identity.status !== 'ready' || !identity.accountAddress ||
          !isAddressEqual(address, identity.accountAddress)) {
        throw new Error('Confirm the canonical Plether Trading Account before requesting funds.')
      }
      if (!identity.isAaManifestConfigured || identity.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID || !identity.ownerAddress || !identity.manifest ||
          !isAddressEqual(identity.manifest.settlementToken, PERPS_ARBITRUM_SEPOLIA.settlementToken) ||
          !isAddressEqual(identity.manifest.marginClearinghouse, PERPS_ARBITRUM_SEPOLIA.marginClearinghouse) ||
          identity.manifest.settlementTokenSupportsEip3009) {
        throw new Error('USDG funding is unavailable for this Trading Account. Please refresh and try again.')
      }

      const started = await perpsApi.claimMarginFunding(identity.ownerAddress, address)
      if (isCancelled()) return
      if (Result.isError(started)) {
        throw new Error(testnetFaucetErrorMessage(started.error), { cause: started.error })
      }
      const initial = started.value.data
      validateFundingEnvelope(initial)
      if (started.value.meta.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID ||
          !isAddressEqual(initial.address as Address, address) ||
          !isAddressEqual(initial.token as Address, PERPS_ARBITRUM_SEPOLIA.settlementToken)) {
        throw new Error('The funding response could not be verified. Refresh and check account activity.')
      }
      if (initial.destination === 'margin') {
        const claimId = initial.claimId
        supportId = claimId
        let progress = initial
        const deadline = Date.now() + 120_000
        while (!isCancelled()) {
          validateMarginFunding(progress, claimId, address)
          failureStage = 'funding_' + progress.phase
          setClaim({
            address, token: progress.token, amount: progress.amount, destination: 'margin',
            txHash: progress.depositTxHash, status: progress.status === 'funded' ? 'margin_funded' : 'submitted',
          })
          if (progress.status === 'funded') {
            setPhase('deposited')
            refreshAccount()
            return
          }
          if (progress.status === 'failed') {
            throw Object.assign(new Error('Plether could not complete this funding step. Retry to resume the same allocation.'), { reason: 'FUNDING_STEP_FAILED' })
          }
          if (progress.status === 'recovery') {
            throw Object.assign(new Error('Your funding needs a recovery check. Contact support with your funding reference.'), { reason: 'FUNDING_RECOVERY_REQUIRED' })
          }
          setPhase(progress.phase === 'mint' ? 'claiming' : progress.phase === 'approval' ? 'approving' : 'depositing')
          if (Date.now() >= deadline) throw Object.assign(new Error('Funding is still processing in the background. Use Check funding to check the same allocation.'), { reason: 'FUNDING_PENDING' })
          await waitForFundingPoll(signal)
          if (isCancelled()) return
          const next = await perpsApi.getMarginFunding(claimId)
          if (isCancelled()) return
          if (Result.isError(next)) throw new Error('Funding status is unavailable. Use Check funding to resume checking the same allocation.', { cause: next.error })
          if (next.value.meta.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID) throw new Error('The funding response could not be verified.')
          progress = next.value.data
        }
        return
      }
      // Only an explicit legacy response can enter the old wallet-funded flow.
      // Failed or unavailable direct funding never falls back to another mint.
      const result = await perpsApi.claimTestnetFaucet(address)
      if (isCancelled()) return
      if (Result.isError(result)) {
        throw new Error(testnetFaucetErrorMessage(result.error), { cause: result.error })
      }
      let funding = result.value.data

      if (result.value.meta.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID ||
          !isAddressEqual(address, identity.accountAddress) ||
          !isAddressEqual(funding.address as Address, address) ||
          !isAddressEqual(funding.token as Address, PERPS_ARBITRUM_SEPOLIA.settlementToken)) {
        throw new Error('The faucet response does not match this Trading Account or deployment. Refresh and try again.')
      }
      setClaim(funding)

      setPhase('waiting')
      if (funding.status === 'submitted' && funding.txHash) {
        if (!isHash(funding.txHash)) throw new Error('The faucet returned an invalid transaction hash.')
        const hash = funding.txHash
        const receiptResult = await Result.tryPromise(() =>
          client.waitForTransactionReceipt({ hash, timeout: 120_000 }),
        )
        if (isCancelled()) return
        if (Result.isError(receiptResult)) {
          throw new Error('Faucet confirmation is unavailable. Your claim is saved. Use Check confirmation to retry; no deposit has been sent.')
        }
        const receipt = receiptResult.value
        if (receipt.status !== 'success') {
          throw new Error(`The funding transaction reverted. Try requesting ${PERPS_SETTLEMENT_TOKEN_LABEL} again.`)
        }
        // Preserve confirmation even if the subsequent balance read or
        // deposit fails; later retries can distinguish consumed funding.
        funding = { ...funding, status: 'minted' }
        setClaim(funding)
      } else if (funding.status === 'submitted') {
        throw new Error(`The faucet has not returned a transaction yet. Try requesting ${PERPS_SETTLEMENT_TOKEN_LABEL} again.`)
      }

      const amount = BigInt(funding.amount)
      if (amount <= 0n) throw new Error('The faucet returned an invalid USDG amount.')
      const deadline = Date.now() + 120_000
      while (!isCancelled()) {
        const balance = await client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.settlementToken,
          abi: ERC20_ABI,
          functionName: 'balanceOf',
          args: [address],
        })
        if (isCancelled()) return
        // A recovered claim may have already been partially deposited or spent.
        const recovered = funding.status === 'already_claimed' || funding.status === 'already_funded'
        if (balance >= amount || (recovered && balance > 0n)) {
          setClaim({ ...funding, status: funding.status === 'submitted' ? 'minted' : funding.status })
          setPhase('depositing')
          await depositMargin(balance < amount ? balance : amount, undefined, 'account')
          if (!isCancelled()) setPhase('deposited')
          return
        }
        if (recovered && balance === 0n) {
          throw new Error('Your test USDG allocation was already claimed and is no longer in this Trading Account. Check your Margin Account and account activity.')
        }
        if (Date.now() >= deadline) {
          throw new Error(`No ${PERPS_SETTLEMENT_TOKEN_LABEL} is available to deposit yet. It may already be deposited. Check your Margin Account or try again shortly.`)
        }
        await new Promise<void>((resolve) => {
          const finish = () => {
            clearTimeout(timer)
            signal.removeEventListener('abort', finish)
            resolve()
          }
          const timer = setTimeout(finish, 2_000)
          signal.addEventListener('abort', finish, { once: true })
        })
      }
    } catch (cause) {
      if (!isCancelled()) {
        setError(reportedTransactionError(cause, cause instanceof Error ? cause.message : 'Unable to fund your Margin Account. Please try again.', { surface: 'perps', action: 'testnet_funding', stage: failureStage, attemptId: supportId }).message)
        setPhase('idle')
      }
    } finally {
      if (operation.current === controller) operation.current = null
    }
  }

  return { claim, phase, error, requestFunds }
}

function validateFundingEnvelope(value: unknown): asserts value is MarginFundingProgress | LegacyMarginFunding {
  const invalid = () => new Error('The funding response could not be verified. Check account activity.')
  if (!value || typeof value !== 'object') throw invalid()
  const data = value as Record<string, unknown>
  if (data.version !== 1 || typeof data.address !== 'string' || typeof data.token !== 'string') throw invalid()
  if (data.destination === 'wallet' && data.status === 'legacy_wallet') return
  if (data.destination !== 'margin' || typeof data.claimId !== 'string' || typeof data.clearinghouse !== 'string' ||
      typeof data.amount !== 'string' || typeof data.phase !== 'string' || typeof data.status !== 'string' ||
      !(data.depositTxHash === null || typeof data.depositTxHash === 'string')) throw invalid()
}

function validateMarginFunding(progress: unknown, claimId: string, address: Address): asserts progress is MarginFundingProgress {
  validateFundingEnvelope(progress)
  if (progress.destination !== 'margin' || progress.claimId !== claimId ||
      !/^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/i.test(claimId) ||
      !isAddressEqual(progress.address as Address, address) ||
      !isAddressEqual(progress.clearinghouse as Address, PERPS_ARBITRUM_SEPOLIA.marginClearinghouse) ||
      !isAddressEqual(progress.token as Address, PERPS_ARBITRUM_SEPOLIA.settlementToken) ||
      progress.amount !== '100000000000' ||
      (progress.status === 'funded' && (progress.phase !== 'deposit' || !progress.depositTxHash || !isHash(progress.depositTxHash))) ||
      !['pending', 'funded', 'failed', 'recovery'].includes(progress.status) ||
      !['mint', 'approval', 'deposit'].includes(progress.phase)) {
    throw new Error('The funding response could not be verified. Check account activity.')
  }
}

function waitForFundingPoll(signal: AbortSignal): Promise<void> {
  return new Promise(resolve => {
    const finish = () => {
      clearTimeout(timer)
      signal.removeEventListener('abort', finish)
      resolve()
    }
    const timer = setTimeout(finish, 5_000)
    signal.addEventListener('abort', finish, { once: true })
  })
}
