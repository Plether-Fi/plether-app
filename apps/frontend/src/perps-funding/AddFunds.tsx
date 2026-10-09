import { useEffect, useRef, useState } from 'react'
import { formatUnits, parseUnits, toHex, type Hex } from 'viem'
import { useAccount, usePublicClient } from 'wagmi'
import { PERPS_ACTIVE_DEPLOYMENT } from '../contracts/perpsAddresses'
import { acquireSponsoredOperationBrowserLane } from '../perps-aa/laneLock'
import type { PerpsIdentityContextValue } from '../perps-aa/PerpsIdentityContext'
import { Button, Input, Modal } from '../components/ui'
import { createFundingApi, type FundingApi } from './api'
import { fetchFundingManifest, fundingManifestUrl } from './manifest'
import { verifyFundingDeployment } from './deployment'
import { assertFundingAccountDepositIdentity } from './accountDeposit'
import { prepareFundingTransactions, sendFundingTransactions, type FundingWallet } from './provider'
import { archiveCompletedFunding, assertSameQuote, fundingReady, fundingNeedsDeposit, fundingSourceFailed, fundingStatusLabel, fundingStorageKey, reconcileFunding, restoreFunding, saveFunding } from './state'
import { address, assertDestination, assertFundingConfig, hash, sameAddress } from './validation'
import type { FundingAccountDepositDestination, FundingDestination, FundingIntent, FundingManifest, FundingQuote, SavedFunding } from './types'

const api = createFundingApi()
const configuredManifestUrl = fundingManifestUrl(import.meta.env.VITE_PERPS_FUNDING_MANIFEST_URL)

export function AddFunds({ identity, onAccountRefresh, onDepositAccount }: { identity: PerpsIdentityContextValue; onAccountRefresh?: () => unknown; onDepositAccount?: (destination: FundingAccountDepositDestination) => void }) {
  const [releaseState, setReleaseState] = useState<{ manifest: FundingManifest; enabled: boolean } | null>(null)
  useEffect(() => {
    if (!configuredManifestUrl) return
    const controller = new AbortController()
    void fetchFundingManifest(configuredManifestUrl, controller.signal).then(async release => {
      let enabled = false
      try { assertFundingConfig(release, await api.config(controller.signal)); enabled = true }
      catch { /* Existing intents remain inspectable while new funding is disabled. */ }
      if (!controller.signal.aborted) setReleaseState({ manifest: release, enabled })
    }).catch(() => { /* A missing or invalid reviewed release keeps the feature off. */ })
    return () => { controller.abort(); }
  }, [])
  if (!releaseState || !identity.ownerAddress) return null
  const { manifest, enabled } = releaseState
  if (!enabled) {
    try { if (!localStorage.getItem(fundingStorageKey(identity.ownerAddress, manifest.releaseId))) return null }
    catch { /* FundingFlow reports unavailable storage without forgetting a transfer. */ }
  }
  return <ConnectedFundingFlow key={`${identity.ownerAddress.toLowerCase()}:${manifest.releaseId}`} identity={identity} manifest={manifest} onAccountRefresh={onAccountRefresh} onDepositAccount={onDepositAccount} />
}

function ConnectedFundingFlow(props: { identity: PerpsIdentityContextValue; manifest: FundingManifest; onAccountRefresh?: () => unknown; onDepositAccount?: (destination: FundingAccountDepositDestination) => void }) {
  const { connector } = useAccount()
  const publicClient = usePublicClient({ chainId: 42161 })
  const wallet = async (): Promise<FundingWallet> => {
    const provider = await connector?.getProvider()
    if (!provider || typeof provider !== 'object' || !('request' in provider) || typeof provider.request !== 'function') throw new Error('Connect a source wallet to continue.')
    return provider as FundingWallet
  }
  return <FundingFlow {...props} api={api} wallet={wallet} verifyDeployment={(quote, destination) => verifyFundingDeployment(publicClient, props.manifest, quote, destination)} />
}

export function FundingFlow({ identity, manifest, api, wallet, verifyDeployment, onAccountRefresh, onDepositAccount }: {
  identity: PerpsIdentityContextValue
  manifest: FundingManifest
  api: FundingApi
  wallet: () => Promise<FundingWallet>
  verifyDeployment: (quote: FundingQuote, destination: FundingDestination) => Promise<void>
  onAccountRefresh?: () => unknown
  onDepositAccount?: (destination: FundingAccountDepositDestination) => void
}) {
  const owner = address(identity.ownerAddress)
  const [open, setOpen] = useState(false)
  const [sourceIndex, setSourceIndex] = useState(0)
  const [inputAmount, setInputAmount] = useState('')
  const [quote, setQuote] = useState<FundingQuote | null>(null)
  const [pinned, setPinned] = useState<FundingDestination | null>(null)
  const [initial] = useState(() => {
    try {
      const restored = restoreFunding(localStorage, owner, manifest.releaseId)
      if (restored && (restored.destination.destinationChainId !== manifest.destinationChainId || !sameAddress(restored.destination.multicallHandler, manifest.multicallHandler) || !sameAddress(restored.destination.destinationSpokePool, manifest.destinationSpokePool) || !sameAddress(restored.destination.clearinghouse, manifest.clearinghouse) || !sameAddress(restored.destination.token, manifest.token))) throw new Error('Saved release bindings changed.')
      return { saved: restored, error: undefined }
    } catch {
      return { saved: null, error: 'Saved funding could not be read. Keep your source transaction hash and contact support before sending again.' }
    }
  })
  const [saved, setSaved] = useState<SavedFunding | null>(initial.saved)
  const [refreshed, setRefreshed] = useState(false)
  const [busy, setBusy] = useState(false)
  const [step, setStep] = useState('')
  const [error, setError] = useState<string | undefined>(initial.error)
  const [manualSourceHash, setManualSourceHash] = useState('')
  const savedRef = useRef<SavedFunding | null>(initial.saved)
  const pendingIntentKey = useRef<string | null>(null)
  const refreshedIntent = useRef<string | null>(null)
  const observationGeneration = useRef(0)
  const mounted = useRef(true)
  const source = manifest.sources[sourceIndex]
  const ready = saved !== null && refreshed && fundingReady(saved.intent)

  function persist(next: SavedFunding, clearPending = false, allowSourceReplacement = false) {
    const key = fundingStorageKey(owner, manifest.releaseId)
    if (localStorage.getItem(`${key}:history:${next.intent.intentId}`)) throw new Error('This completed funding intent was archived. Reload to view the active transfer.')
    const persisted = restoreFunding(localStorage, owner, manifest.releaseId)
    if (persisted) {
      if (persisted.intent.intentId !== next.intent.intentId) throw new Error('Another funding intent is active. Reload to resume it.')
      const merged = reconcileFunding(persisted, next.intent, allowSourceReplacement)
      const sourceTxHash = next.sourceTxHash ?? merged.sourceTxHash
      if (sourceTxHash && merged.sourceTxHash && sourceTxHash.toLowerCase() !== merged.sourceTxHash.toLowerCase()) throw new Error('The source transaction changed.')
      // Background polling in another tab must never erase a pending wallet
      // request or a broadcast hash written by the tab holding the send lock.
      next = { ...next, sourceTxHash, sourceSubmissionPending: sourceTxHash ? false : clearPending ? false : (next.sourceSubmissionPending === true || persisted.sourceSubmissionPending === true) }
    }
    saveFunding(localStorage, next)
    savedRef.current = next
    if (mounted.current) setSaved(next)
  }
  function accept(next: FundingIntent, allowSourceReplacement = false) {
    const current = savedRef.current
    if (!current) throw new Error('The saved funding intent is unavailable.')
    persist(reconcileFunding(current, next, allowSourceReplacement), false, allowSourceReplacement)
    if (mounted.current) setRefreshed(true)
    const refreshedKey = `${next.intentId}:${next.status}:${next.depositTxHash ?? next.fallbackTxHash ?? ''}`
    if ((fundingReady(next) || fundingNeedsDeposit(next)) && refreshedIntent.current !== refreshedKey) {
      refreshedIntent.current = refreshedKey
      void Promise.resolve().then(() => onAccountRefresh?.()).catch(() => {
        if (mounted.current) setError('Funding evidence updated. Refresh your Trading Account to update its displayed balance.')
      })
    }
  }

  useEffect(() => {
    mounted.current = true
    return () => { mounted.current = false }
  }, [owner, manifest.releaseId])

  useEffect(() => {
    if (!saved?.intent.intentId) return
    const id = saved.intent.intentId
    let active = true
    const controller = new AbortController()
    const refresh = () => {
      const generation = ++observationGeneration.current
      void api.intent(id, controller.signal).then(next => {
        if (active && generation === observationGeneration.current) accept(next)
      }).catch((e: unknown) => {
        if (active && generation === observationGeneration.current) { setRefreshed(false); setError(message(e)) }
      })
    }
    refresh()
    const timer = setInterval(refresh, 10_000)
    return () => { active = false; controller.abort(); clearInterval(timer) }
    // Poll one immutable id. accept reads the latest persisted state through savedRef.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [saved?.intent.intentId, api])

  async function action(run: () => Promise<void>) {
    if (busy || initial.error) return
    setBusy(true)
    setError(undefined)
    try { await run() } catch (e) { if (mounted.current) setError(message(e)) }
    finally { if (mounted.current) { setBusy(false); setStep('') } }
  }

  async function requestQuote() {
    if (PERPS_ACTIVE_DEPLOYMENT.chainId !== manifest.destinationChainId || PERPS_ACTIVE_DEPLOYMENT.releaseId !== manifest.releaseId || !sameAddress(PERPS_ACTIVE_DEPLOYMENT.contracts.usdc, manifest.token) || !sameAddress(PERPS_ACTIVE_DEPLOYMENT.contracts.marginClearinghouse, manifest.clearinghouse)) throw new Error('The active trading release does not match this funding destination.')
    const aa = identity.manifest
    if (identity.status !== 'ready' || !identity.accountAddress || aa?.chainId !== manifest.destinationChainId || !sameAddress(aa.usdc, manifest.token) || !sameAddress(aa.marginClearinghouse, manifest.clearinghouse)) throw new Error('Return to your reviewed destination Trading Account before starting a new funding intent.')
    if (!/^\d+(\.\d{1,6})?$/.test(inputAmount) || parseUnits(inputAmount, source.decimals) <= 0n) throw new Error('Enter a positive source-token amount with no more than six decimals.')
    assertFundingConfig(manifest, await api.config())
    const destination: FundingDestination = { owner, beneficiary: identity.accountAddress, destinationChainId: manifest.destinationChainId, token: manifest.token, clearinghouse: manifest.clearinghouse, multicallHandler: manifest.multicallHandler, destinationSpokePool: manifest.destinationSpokePool, releaseId: manifest.releaseId }
    assertFundingAccountDepositIdentity(destination, identity)
    const next = await api.quote({ ownerAddress: owner, beneficiary: destination.beneficiary, sourceChainId: source.chainId, sourceToken: source.token, sourceAmount: parseUnits(inputAmount, source.decimals).toString() })
    assertDestination(next, destination)
    if (next.sourceChainId !== source.chainId || !sameAddress(next.sourceToken, source.token) || next.sourceAmount !== parseUnits(inputAmount, source.decimals).toString()) throw new Error('The quote source changed.')
    prepareFundingTransactions(next, destination, manifest)
    await verifyDeployment(next, destination)
    setPinned(destination)
    setQuote(next)
    pendingIntentKey.current = crypto.randomUUID()
  }

  async function fund() {
    const release = await acquireSponsoredOperationBrowserLane({ chainId: manifest.destinationChainId, accountAddress: owner, lane: `funding:${manifest.releaseId}` })
    try { await fundLocked() } finally { await release() }
  }

  async function fundLocked() {
    // A second tab may have sent while this tab still showed an unsent intent.
    const persisted = restoreFunding(localStorage, owner, manifest.releaseId)
    if (persisted) {
      if (savedRef.current && persisted.intent.intentId !== savedRef.current.intent.intentId) throw new Error('Another funding intent is active. Reload to resume it.')
      savedRef.current = persisted
      setSaved(persisted)
    } else if (savedRef.current) throw new Error('This funding intent was archived in another tab. Reload before continuing.')
    let current = savedRef.current
    if (!current) {
      if (!quote || !pinned || !pendingIntentKey.current) throw new Error('Request a new quote first.')
      assertFundingConfig(manifest, await api.config())
      prepareFundingTransactions(quote, pinned, manifest)
      const intent = await api.createIntent(quote.quoteId, pendingIntentKey.current)
      assertDestination(intent, pinned)
      assertSameQuote(quote, intent)
      current = { version: 1, destination: pinned, intent }
      persist(current)
      setRefreshed(true)
    }
    if (current.sourceTxHash || current.intent.sourceTxHash || current.sourceSubmissionPending || current.intent.status !== 'awaiting-source') throw new Error('This transfer has already started. Refresh its status instead of sending again.')
    const active = current
    assertFundingConfig(manifest, await api.config())
    await verifyDeployment(active.intent, active.destination)
    try {
      const sourceTxHash = await sendFundingTransactions({
        wallet: await wallet(), destination: active.destination, quote: active.intent, manifest,
        onStep: setStep,
        beforeBridge: () => { persist({ ...(savedRef.current ?? active), sourceSubmissionPending: true }); },
        onBridgeHash: txHash => { persist({ ...(savedRef.current ?? active), sourceTxHash: txHash, sourceSubmissionPending: false }); },
      })
      await reportSource(sourceTxHash)
    } catch (e) {
      // Only an explicit wallet rejection proves that a requested send did not broadcast.
      if (typeof e === 'object' && e !== null && 'code' in e && e.code === 4001) persist({ ...(savedRef.current ?? active), sourceSubmissionPending: false }, true)
      throw e
    }
  }

  async function refreshIntent() {
    if (!savedRef.current) return
    const generation = ++observationGeneration.current
    try {
      const next = await api.intent(savedRef.current.intent.intentId)
      if (generation === observationGeneration.current) accept(next)
    } catch (e) {
      if (generation === observationGeneration.current) { setRefreshed(false); throw e }
    }
  }

  async function reportSource(txHash: Hex) {
    if (!savedRef.current) return
    const next = await api.sourceSubmitted(savedRef.current.intent.intentId, txHash)
    // A manually entered hash is only durable after the API accepts it. A typo or
    // rejected candidate must leave the recovery input editable.
    if (next.sourceTxHash?.toLowerCase() !== txHash.toLowerCase()) throw new Error('The backend did not accept the submitted source transaction.')
    // Registration proves the source hash, not the freshness of destination
    // evidence bundled with its response. Invalidate older reads and wait for
    // the next canonical GET before presenting readiness or archival actions.
    ++observationGeneration.current
    persist({ ...reconcileFunding(savedRef.current, next, true), sourceSubmissionPending: false }, true, true)
    setRefreshed(false)
  }

  async function startAnother() {
    if (!saved || !refreshed || (!ready && !fundingNeedsDeposit(saved.intent) && !fundingSourceFailed(saved.intent))) return
    const release = await acquireSponsoredOperationBrowserLane({ chainId: manifest.destinationChainId, accountAddress: owner, lane: `funding:${manifest.releaseId}` })
    try {
      const latest = restoreFunding(localStorage, owner, manifest.releaseId)
      if (latest?.intent.intentId !== saved.intent.intentId) throw new Error('The active funding intent changed in another tab. Reload to continue.')
      let next: FundingIntent
      const generation = ++observationGeneration.current
      try { next = await api.intent(latest.intent.intentId) }
      catch (e) { if (generation === observationGeneration.current) setRefreshed(false); throw e }
      if (generation !== observationGeneration.current) throw new Error('A newer funding observation is in progress. Refresh the transfer before archiving.')
      const checked = reconcileFunding(latest, next)
      persist(checked)
      setRefreshed(true)
      archiveCompletedFunding(localStorage, checked)
    } finally { await release() }
    savedRef.current = null
    setSaved(null)
    setQuote(null)
    setRefreshed(false)
    setInputAmount('')
  }

  async function returnToTrading() {
    const connected = await wallet()
    const accounts = await connected.request({ method: 'eth_accounts' })
    if (!Array.isArray(accounts) || typeof accounts[0] !== 'string' || !sameAddress(accounts[0], owner)) throw new Error('Reconnect the wallet that owns this Trading Account.')
    await connected.request({ method: 'wallet_switchEthereumChain', params: [{ chainId: toHex(manifest.destinationChainId) }] })
    identity.reloadIdentity()
  }

  async function dismissUnsent() {
    const release = await acquireSponsoredOperationBrowserLane({ chainId: manifest.destinationChainId, accountAddress: owner, lane: `funding:${manifest.releaseId}` })
    try {
      const latest = restoreFunding(localStorage, owner, manifest.releaseId)
      if (!latest || latest.sourceSubmissionPending || latest.sourceTxHash || latest.intent.sourceTxHash || latest.intent.status !== 'awaiting-source') throw new Error('This transfer may have started in another tab. Refresh its status.')
      localStorage.removeItem(fundingStorageKey(owner, manifest.releaseId))
    } finally { await release() }
    savedRef.current = null
    setSaved(null)
    setQuote(null)
    setRefreshed(false)
  }

  return <>
    <Button type="button" variant="secondary" className="w-full" onClick={() => { setOpen(true); }}>{saved && !ready ? 'View funding transfer' : 'Add funds'}</Button>
    <Modal isOpen={open} onClose={() => { setOpen(false); }} title="Add funds" size="md">
      <div className="space-y-4">
        <p className="text-sm text-content-secondary">Bridge Ethereum USDC or USDT into your Arbitrum USDC margin balance. Source approvals and the transfer require ETH in your source wallet.</p>
        {saved ? <>
          <p role="status" className="font-semibold">{busy && step ? step : fundingStatusLabel(saved.intent, refreshed)}</p>
          <p className="text-sm">Trading Account <code className="break-all">{saved.destination.beneficiary}</code></p>
          <p className="text-xs text-content-secondary">Destination remains Arbitrum (chain {saved.destination.destinationChainId}) when your wallet changes networks.</p>
          <p className="text-xs break-all">Funding reference: {saved.intent.intentId}</p>
          <p className="text-xs break-all">Destination handler: {saved.intent.multicallHandler}</p>
          {(saved.sourceTxHash ?? saved.intent.sourceTxHash) && <p className="text-xs break-all">Source transaction: {saved.sourceTxHash ?? saved.intent.sourceTxHash}</p>}
          {saved.intent.depositTxHash && <p className="text-xs break-all">Margin deposit transaction: {saved.intent.depositTxHash}</p>}
          {ready ? <p>{formatUnits(BigInt(saved.intent.creditedAmount ?? '0'), 6)} USDC confirmed in your clearinghouse balance.</p> : <p className="text-sm">Bridged funds are available to trade only after a canonical clearinghouse deposit is confirmed.</p>}
          <div className="flex gap-2"><Button disabled={busy} onClick={() => void action(returnToTrading)}>Return to Arbitrum</Button>{refreshed && (ready || fundingNeedsDeposit(saved.intent) || fundingSourceFailed(saved.intent)) && <Button disabled={busy} variant="secondary" onClick={() => void action(startAnother)}>{ready ? 'Add more funds' : fundingNeedsDeposit(saved.intent) ? 'Archive returned transfer & start new quote' : 'Archive failed transfer & start new quote'}</Button>}</div>
          {saved.intent.sourceStatus === 'reverted' && !ready && <p className="text-sm">The source transaction reverted. That source transaction did not bridge funds; source gas may still have been spent. Keep this funding reference and verify the transaction in your source wallet.</p>}
          {saved.intent.bridgeStatus === 'expired' && !ready && <p className="text-sm">The provider reports the bridge deadline has passed. Keep your source transaction hash and check the source wallet for refund progress before starting another transfer.</p>}
          {saved.intent.bridgeStatus === 'refunded' && !ready && <p className="text-sm">The provider reports a refund to your source wallet. This does not credit your margin balance. Verify the refund in your source wallet.</p>}
          {saved.intent.reason && <p className="text-sm">{saved.intent.reason}</p>}
          {saved.intent.status === 'awaiting-source' && !saved.sourceSubmissionPending && !saved.sourceTxHash && !saved.intent.sourceTxHash && <div className="flex gap-2"><Button disabled={busy} onClick={() => void action(fund)}>Continue in wallet</Button><Button disabled={busy} variant="secondary" onClick={() => { void action(dismissUnsent) }}>New quote</Button></div>}
          {(!ready && ((saved.sourceSubmissionPending === true && !saved.sourceTxHash) || (saved.sourceTxHash !== undefined && !saved.intent.sourceTxHash) || saved.intent.sourceStatus === 'pending')) && <div className="space-y-2"><p className="text-sm">{saved.sourceSubmissionPending && !saved.sourceTxHash ? 'A wallet request was interrupted. Check wallet history before sending again. Paste the existing source transaction hash to resume tracking.' : 'If your wallet replaced the source transaction, enter the replacement hash after it is visible on Ethereum. The backend verifies that it sends the same reviewed bridge call.'}</p><Input label="Source transaction hash" value={manualSourceHash} onChange={e => { setManualSourceHash(e.target.value); }} /><Button disabled={busy} onClick={() => void action(() => reportSource(hash(manualSourceHash)))}>Track existing transfer</Button></div>}
          {saved.sourceTxHash && !saved.intent.sourceTxHash && <Button disabled={busy} onClick={() => void action(() => reportSource(hash(saved.sourceTxHash)))}>Resume transfer tracking</Button>}
          {refreshed && fundingNeedsDeposit(saved.intent) && <div className="space-y-2">
            <p className="text-sm">The destination action failed. {formatUnits(BigInt(saved.intent.fallbackAmount ?? '0'), 6)} USDC was returned to your Trading Account in transaction <code className="break-all">{saved.intent.fallbackTxHash}</code>. That return did not credit margin; these tokens may since have been spent or deposited.</p>
            {onDepositAccount && <Button disabled={busy} onClick={() => void action(async () => {
              assertFundingAccountDepositIdentity(saved.destination, identity)
              await onAccountRefresh?.()
              onDepositAccount(saved.destination)
              setOpen(false)
            })}>Review Trading Account deposit</Button>}
            <p className="text-sm text-content-secondary">Return to Arbitrum and review the account's current USDC balance in Deposit. Archiving this returned transfer keeps its history and does not deposit these tokens or make them ready to trade.</p>
          </div>}
          <Button variant="secondary" disabled={busy} onClick={() => void action(refreshIntent)}>Refresh transfer status</Button>
        </> : <>
          <label className="block text-sm">Source asset<select aria-label="Source asset" className="mt-1 w-full rounded border bg-app-bg p-2" value={sourceIndex} onChange={e => { setSourceIndex(Number(e.target.value)); setQuote(null) }} disabled={busy || Boolean(initial.error)}>{manifest.sources.map((s, i) => <option key={`${s.chainId.toString()}:${s.token}`} value={i}>{s.name} {s.symbol}</option>)}</select></label>
          <Input label={`Amount (${source.symbol})`} disabled={busy || Boolean(initial.error)} value={inputAmount} onChange={e => { setInputAmount(e.target.value); setQuote(null) }} />
          {quote && pinned ? <><p className="text-sm">Estimated arrival: {formatUnits(BigInt(quote.estimatedAmount), 6)} USDC<br />Minimum arrival: {formatUnits(BigInt(quote.minimumAmount), 6)} USDC</p><p className="text-xs break-all">Trading Account: {pinned.beneficiary}<br />Destination handler: {quote.multicallHandler}<br />Quote expires: {new Date(quote.expiresAt * 1000).toLocaleTimeString()}</p><Button disabled={busy} onClick={() => void action(fund)}>{busy ? step || 'Preparing transfer' : 'Approve & send from wallet'}</Button></> : <Button disabled={busy} onClick={() => void action(requestQuote)}>{busy ? 'Finding route' : 'Review funding quote'}</Button>}
        </>}
        {(error ?? initial.error) && <p role="alert" className="text-sm text-brand-orange">{error ?? initial.error}</p>}
      </div>
    </Modal>
  </>
}

function message(error: unknown): string { return error instanceof Error ? error.message : 'Funding could not continue. Check your existing transfer before trying again.' }
