import { reportedTransactionError } from '../analytics/transactionErrors'
import { useEffect, useRef, useState } from 'react'
import { Result } from 'better-result'
import { isAddressEqual, isHash, keccak256, parseAbi, type Address } from 'viem'
import { usePublicClient, useWriteContract } from 'wagmi'
import { perpsApi, testnetFaucetErrorMessage } from '../api'
import type { TestnetFaucetClaim } from '../api/types'
import { ERC20_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, PERPS_SETTLEMENT_TOKEN, PERPS_SETTLEMENT_TOKEN_CONTRACT, PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'
import { usePerpsIdentity } from '../perps-aa'
import { usePerpsTrading } from './usePerpsTrading'

const MINT_ABI = parseAbi(['function mint(address recipient, uint256 amount)'])
const TESTNET_FUNDING_AMOUNT = 100_000_000_000n

export type TestnetFundingPhase = 'idle' | 'claiming' | 'waiting' | 'depositing' | 'deposited'

export function useTestnetFunding(recipient: string, enabled: boolean) {
  const identity = usePerpsIdentity()
  const client = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const { writeContractAsync } = useWriteContract()
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
      const directMockMint = PERPS_SETTLEMENT_TOKEN.isMock && PERPS_SETTLEMENT_TOKEN.symbol === 'USDG'
      if (identity.isAaManifestConfigured || directMockMint) {
        if (identity.status !== 'ready' || !identity.accountAddress ||
            !isAddressEqual(address, identity.accountAddress)) {
          throw new Error('Confirm the canonical Plether Trading Account before requesting funds.')
        }
      }
      let funding: TestnetFaucetClaim
      if (directMockMint) {
        // Unrestricted test minting is a regular owner-wallet transaction. It
        // never signs EIP-3009 or consumes the backend faucet entitlement.
        if (!identity.isAaManifestConfigured || identity.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID || !identity.ownerAddress || !identity.accountAddress || !identity.manifest ||
            !isAddressEqual(identity.manifest.usdc, PERPS_ARBITRUM_SEPOLIA.usdc) ||
            !isAddressEqual(identity.manifest.marginClearinghouse, PERPS_ARBITRUM_SEPOLIA.marginClearinghouse) ||
            identity.manifest.usdcSupportsEip3009) {
          throw new Error('The Mock USDG funding configuration does not match this Trading Account.')
        }
        let reusableClaim = claim && phase !== 'deposited' && isAddressEqual(claim.address as Address, address) ? claim : null
        if (reusableClaim?.status === 'minted' || reusableClaim?.status === 'already_funded') {
          const remaining = await client.readContract({
            address: PERPS_ARBITRUM_SEPOLIA.usdc, abi: ERC20_ABI,
            functionName: 'balanceOf', args: [identity.accountAddress],
          })
          if (isCancelled()) return
          // The previous deposit may have confirmed after a timeout, or the
          // user may have moved these funds manually. An explicit new request
          // can mint again once that confirmed funding has been consumed.
          const originalAmount = BigInt(reusableClaim.amount)
          reusableClaim = remaining > 0n
            ? { ...reusableClaim, amount: (remaining < originalAmount ? remaining : originalAmount).toString(), status: 'already_funded' }
            : null
        }
        if (reusableClaim) {
          funding = reusableClaim
        } else {
          const code = await client.getCode({ address: PERPS_ARBITRUM_SEPOLIA.usdc })
          if (isCancelled()) return
          if (!code || code === '0x' || keccak256(code) !== PERPS_SETTLEMENT_TOKEN_CONTRACT.runtimeCodeHash.toLowerCase()) {
            throw new Error('Mock USDG bytecode does not match the reviewed release. No mint was sent.')
          }
          const txHash = await writeContractAsync({
            account: identity.ownerAddress,
            chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
            address: PERPS_ARBITRUM_SEPOLIA.usdc,
            abi: MINT_ABI,
            functionName: 'mint',
            args: [identity.accountAddress, TESTNET_FUNDING_AMOUNT],
          })
          if (isCancelled()) return
          funding = { address, token: PERPS_ARBITRUM_SEPOLIA.usdc, amount: TESTNET_FUNDING_AMOUNT.toString(), txHash, status: 'submitted' }
        }
      } else {
        const result = await perpsApi.claimTestnetFaucet(address)
        if (isCancelled()) return
        if (Result.isError(result)) throw new Error(testnetFaucetErrorMessage(result.error))
        funding = result.value.data
      }
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
          if (directMockMint) setClaim(null)
          throw new Error(`The funding transaction reverted. Try requesting ${PERPS_SETTLEMENT_TOKEN_LABEL} again.`)
        }
        if (directMockMint && funding.status === 'submitted') {
          // Preserve confirmation even if the subsequent balance read or
          // deposit fails; later retries can distinguish consumed funding.
          funding = { ...funding, status: 'minted' }
          setClaim(funding)
        }
      } else if (funding.status === 'submitted') {
        throw new Error(`The faucet has not returned a transaction yet. Try requesting ${PERPS_SETTLEMENT_TOKEN_LABEL} again.`)
      }

      const amount = BigInt(funding.amount)
      if (amount <= 0n) throw new Error('The faucet returned an invalid settlement token amount.')
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
        setError(reportedTransactionError(cause, cause instanceof Error ? cause.message : 'Unable to fund your Margin Account. Please try again.', { surface: 'perps', action: 'testnet_funding', stage: 'funding' }).message)
        setPhase('idle')
      }
    } finally {
      if (operation.current === controller) operation.current = null
    }
  }

  return { claim, phase, error, requestFunds }
}
