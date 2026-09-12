import { useEffect, useRef, useState } from 'react'
import { Result } from 'better-result'
import { isAddressEqual, isHash, type Address } from 'viem'
import { usePublicClient } from 'wagmi'
import { perpsApi, testnetFaucetErrorMessage } from '../api'
import type { TestnetFaucetClaim } from '../api/types'
import { ERC20_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { usePerpsIdentity } from '../perps-aa'
import { usePerpsTrading } from './usePerpsTrading'

export type TestnetFundingPhase = 'idle' | 'claiming' | 'waiting' | 'depositing' | 'deposited'

export function useTestnetFunding(recipient: string, enabled: boolean) {
  const identity = usePerpsIdentity()
  const client = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const { depositMargin } = usePerpsTrading()
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
    if (!enabled || operation.current) return
    const controller = new AbortController()
    operation.current = controller
    const { signal } = controller
    const isCancelled = () => signal.aborted
    setError(null)
    setPhase('claiming')

    try {
      const result = await perpsApi.claimTestnetFaucet(address)
      if (isCancelled()) return
      if (Result.isError(result)) throw new Error(testnetFaucetErrorMessage(result.error))
      const funding = result.value.data
      setClaim(funding)

      // Legacy wallet recipients keep the manual deposit flow.
      if (!identity.isAaManifestConfigured) {
        setPhase('idle')
        return
      }
      if (identity.status !== 'ready' || !identity.accountAddress ||
          !isAddressEqual(address, identity.accountAddress) ||
          !isAddressEqual(funding.address as Address, address) ||
          !isAddressEqual(funding.token as Address, PERPS_ARBITRUM_SEPOLIA.usdc)) {
        throw new Error('The faucet recipient or token does not match this Trading Account. Refresh and try again.')
      }

      setPhase('waiting')
      if (funding.txHash) {
        if (!isHash(funding.txHash)) throw new Error('The faucet returned an invalid transaction hash.')
        const receipt = await client.waitForTransactionReceipt({ hash: funding.txHash, timeout: 120_000 })
        if (isCancelled()) return
        if (receipt.status !== 'success') throw new Error('The faucet transaction reverted. Try requesting mock USDC again.')
      } else if (funding.status === 'submitted') {
        throw new Error('The faucet has not returned a transaction yet. Try requesting mock USDC again.')
      }

      const amount = BigInt(funding.amount)
      if (amount <= 0n) throw new Error('The faucet returned an invalid mock USDC amount.')
      const deadline = Date.now() + 120_000
      while (!isCancelled()) {
        const balance = await client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.usdc,
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
        if (Date.now() >= deadline) {
          throw new Error('No mock USDC is available to deposit yet. It may already be deposited. Check your Margin Account or try again shortly.')
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
        setError(cause instanceof Error ? cause.message : 'Unable to fund your Margin Account. Please try again.')
        setPhase('idle')
      }
    } finally {
      if (operation.current === controller) operation.current = null
    }
  }

  return { claim, phase, error, requestFunds }
}
