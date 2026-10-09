import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import publicAaManifest from '../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../perps-aa/manifest'
import { PERPS_ACTIVE_DEPLOYMENT } from '../contracts/perpsAddresses'
import type { PerpsIdentityContextValue } from '../perps-aa/PerpsIdentityContext'
import { acquireSponsoredOperationBrowserLane } from '../perps-aa/laneLock'
import { FundingFlow } from './AddFunds'
import type { FundingApi } from './api'
import { prepareFundingTransactions, sendFundingTransactions, type FundingWallet } from './provider'
import { fundingStorageKey, restoreFunding, saveFunding } from './state'
import { parseFundingConfig, parseFundingManifest } from './validation'
import type { FundingAccountDepositDestination, FundingDestination, FundingIntent, FundingQuote } from './types'
import { BENEFICIARY, BLOCK_HASH, CLEARINGHOUSE, DESTINATION_USDC, OTHER_ADDRESS, OWNER, SOURCE_HASH, QUOTE_ID, MULTICALL_HANDLER, DESTINATION_SPOKE_POOL, confirmedIntentFixture, intentFixture, quoteFixture, releaseFixture, savedFixture, terminalSourceIntentFixture, needsDepositIntentFixture } from './testFixtures'

vi.mock('../perps-aa/laneLock', () => ({ acquireSponsoredOperationBrowserLane: vi.fn() }))
vi.mock('./provider', () => ({ prepareFundingTransactions: vi.fn(), sendFundingTransactions: vi.fn() }))
vi.mock('../contracts/perpsAddresses', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../contracts/perpsAddresses')>()
  const { isPerpsManifestForDeployment } = await import('../contracts/perpsDeployment')
  const deployment = {
    ...actual.PERPS_ACTIVE_DEPLOYMENT,
    chainId: 42161 as const,
    releaseId: 'perps-arbitrum-funding-reviewed-test-fixture',
    contracts: {
      ...actual.PERPS_ACTIVE_DEPLOYMENT.contracts,
      usdc: '0xaf88d065e77c8cc2239327c5edb3a432268e5831' as const,
      marginClearinghouse: '0x3333333333333333333333333333333333333333' as const,
    },
  }
  return {
    ...actual,
    PERPS_ACTIVE_DEPLOYMENT: deployment,
    isPerpsManifestForActiveDeployment: (manifest: Parameters<typeof actual.isPerpsManifestForActiveDeployment>[0]) => isPerpsManifestForDeployment(manifest, deployment),
  }
})

const release = parseFundingManifest(releaseFixture())
const verifyDeployment = vi.fn<(quote: FundingQuote, destination: FundingDestination) => Promise<void>>()

function deferred<T>() {
  let resolve!: (value: T) => void
  const promise = new Promise<T>((yes) => { resolve = yes })
  return { promise, resolve }
}

function identityFixture(overrides: Partial<PerpsIdentityContextValue> = {}): PerpsIdentityContextValue {
  return {
    status: 'ready', ownerAddress: OWNER, accountAddress: BENEFICIARY, chainId: 42161,
    isAaManifestConfigured: true, sponsorshipEnabled: false,
    manifest: { ...parsePerpsAaManifest(publicAaManifest), ...PERPS_ACTIVE_DEPLOYMENT.contracts, chainId: 42161, usdc: DESTINATION_USDC, marginClearinghouse: CLEARINGHOUSE },
    identity: null, proposedIdentity: null, changedIdentityFields: [], error: null,
    confirmIdentityAfterContinuityCheck: () => true, reloadIdentity: vi.fn(),
    ...overrides,
  }
}

function apiFixture() {
  return {
    config: vi.fn<FundingApi['config']>().mockResolvedValue(parseFundingConfig({ ...releaseFixture(), provider: 'across', enabled: true })),
    quote: vi.fn<FundingApi['quote']>().mockResolvedValue(quoteFixture()),
    createIntent: vi.fn<FundingApi['createIntent']>().mockResolvedValue(intentFixture()),
    intent: vi.fn<FundingApi['intent']>().mockResolvedValue(intentFixture()),
    sourceSubmitted: vi.fn<FundingApi['sourceSubmitted']>().mockResolvedValue(intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH })),
  }
}

function walletFixture() {
  const request = vi.fn<FundingWallet['request']>()
  return vi.fn<() => Promise<FundingWallet>>().mockResolvedValue({ request })
}

beforeEach(() => {
  vi.resetAllMocks()
  verifyDeployment.mockResolvedValue(undefined)
  vi.mocked(acquireSponsoredOperationBrowserLane).mockResolvedValue(vi.fn(async () => {}))
  vi.mocked(prepareFundingTransactions).mockReturnValue([])
})

afterEach(() => {
  cleanup()
  vi.useRealTimers()
})

describe('funding transfer recovery UI', () => {
  it('restores the pinned destination while the source wallet network blocks the trading identity', async () => {
    saveFunding(localStorage, savedFixture(confirmedIntentFixture()))
    const pending = deferred<FundingIntent>()
    const api = apiFixture()
    api.intent.mockReturnValue(pending.promise)
    const wallet = walletFixture()
    const refreshAccount = vi.fn()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture({ status: 'blocked', accountAddress: undefined, chainId: 1, manifest: null })} manifest={release} api={api} wallet={wallet} onAccountRefresh={refreshAccount} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))

    expect(screen.getByRole('status')).toHaveTextContent('Checking saved transfer')
    expect(screen.getByText(BENEFICIARY)).toBeInTheDocument()
    expect(screen.getByText(/Destination remains Arbitrum \(chain 42161\)/)).toBeInTheDocument()
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
    expect(refreshAccount).not.toHaveBeenCalled()
    expect(wallet).not.toHaveBeenCalled()

    await act(async () => { pending.resolve(confirmedIntentFixture()) })
    expect(screen.getByRole('status')).toHaveTextContent('Ready to trade')
    expect(screen.getByText('99 USDC confirmed in your clearinghouse balance.')).toBeInTheDocument()
    expect(refreshAccount).toHaveBeenCalledTimes(1)
    expect(api.intent).toHaveBeenCalledWith('intent-test-1', expect.any(AbortSignal))
  })

  it('removes readiness when a later canonical read retracts a deposit after a reorg', async () => {
    vi.useFakeTimers()
    saveFunding(localStorage, savedFixture(confirmedIntentFixture()))
    const api = apiFixture()
    api.intent.mockResolvedValue(confirmedIntentFixture())
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    await act(async () => {})
    expect(screen.getByRole('status')).toHaveTextContent('Ready to trade')

    api.intent.mockResolvedValue(intentFixture({ status: 'depositing', sourceTxHash: SOURCE_HASH }))
    await act(async () => { await vi.advanceTimersByTimeAsync(10_000) })
    expect(screen.getByRole('status')).toHaveTextContent('Confirming margin deposit')
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Add more funds' })).not.toBeInTheDocument()
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.intent.depositBlockHash).toBeUndefined()
  })

  it('suppresses a previously confirmed result when canonical status is unavailable', async () => {
    vi.useFakeTimers()
    saveFunding(localStorage, savedFixture(confirmedIntentFixture()))
    const api = apiFixture()
    api.intent.mockResolvedValue(confirmedIntentFixture())
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    await act(async () => {})
    expect(screen.getByRole('status')).toHaveTextContent('Ready to trade')

    api.intent.mockRejectedValue(new Error('Canonical confirmation service is unavailable.'))
    await act(async () => { await vi.advanceTimersByTimeAsync(10_000) })
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
    expect(screen.getByRole('status')).toHaveTextContent('Checking saved transfer')
    expect(screen.getByRole('alert')).toHaveTextContent('Canonical confirmation service is unavailable.')
    expect(screen.queryByRole('button', { name: 'Add more funds' })).not.toBeInTheDocument()
  })

  it('immediately clears readiness when a manual refresh fails, without waiting for a poll', async () => {
    saveFunding(localStorage, savedFixture(confirmedIntentFixture()))
    const api = apiFixture()
    api.intent.mockResolvedValue(confirmedIntentFixture())
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Ready to trade'))

    api.intent.mockRejectedValue(new Error('Canonical status is unavailable.'))
    fireEvent.click(screen.getByRole('button', { name: 'Refresh transfer status' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('Canonical status is unavailable.'))
    expect(screen.getByRole('status')).toHaveTextContent('Checking saved transfer')
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Add more funds' })).not.toBeInTheDocument()
    expect(api.intent).toHaveBeenCalledTimes(2)
  })

  it('archives completed evidence under the browser lock before starting another deposit', async () => {
    saveFunding(localStorage, savedFixture(confirmedIntentFixture()))
    const api = apiFixture()
    api.intent.mockResolvedValue(confirmedIntentFixture())
    const releaseLock = vi.fn(async () => {})
    vi.mocked(acquireSponsoredOperationBrowserLane).mockResolvedValue(releaseLock)
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    expect(screen.queryByRole('button', { name: 'Add more funds' })).not.toBeInTheDocument()
    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Ready to trade'))
    fireEvent.click(screen.getByRole('button', { name: 'Add more funds' }))
    await waitFor(() => expect(screen.getByRole('textbox', { name: 'Amount (USDC)' })).toHaveValue(''))

    expect(restoreFunding(localStorage, OWNER, release.releaseId)).toBeNull()
    const archive = JSON.parse(localStorage.getItem(`${fundingStorageKey(OWNER, release.releaseId)}:history:intent-test-1`)!)
    expect(archive).toMatchObject({ intent: { intentId: 'intent-test-1', status: 'confirmed', sourceTxHash: SOURCE_HASH, depositTxHash: confirmedIntentFixture().depositTxHash } })
    expect(acquireSponsoredOperationBrowserLane).toHaveBeenCalledWith({ chainId: 42161, accountAddress: OWNER, lane: `funding:${release.releaseId}` })
    expect(releaseLock).toHaveBeenCalledTimes(1)
    expect(sendFundingTransactions).not.toHaveBeenCalled()
    expect(api.createIntent).not.toHaveBeenCalled()
  })

  it('rechecks terminal source-failure evidence before archiving and allowing a new quote', async () => {
    const failed = terminalSourceIntentFixture()
    saveFunding(localStorage, savedFixture(failed))
    const firstRead = deferred<FundingIntent>()
    const finalRead = deferred<FundingIntent>()
    const api = apiFixture()
    api.intent.mockReturnValueOnce(firstRead.promise).mockReturnValueOnce(finalRead.promise)
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    expect(screen.queryByRole('button', { name: 'Archive failed transfer & start new quote' })).not.toBeInTheDocument()
    await act(async () => { firstRead.resolve(failed) })
    expect(screen.getByRole('status')).toHaveTextContent('Source transaction reverted')
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Archive failed transfer & start new quote' }))
    await waitFor(() => expect(api.intent).toHaveBeenCalledTimes(2))
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.intent.intentId).toBe(failed.intentId)
    expect(screen.queryByRole('textbox', { name: 'Amount (USDC)' })).not.toBeInTheDocument()
    await act(async () => { finalRead.resolve(failed) })
    expect(screen.getByRole('textbox', { name: 'Amount (USDC)' })).toHaveValue('')
    expect(restoreFunding(localStorage, OWNER, release.releaseId)).toBeNull()
    expect(JSON.parse(localStorage.getItem(`${fundingStorageKey(OWNER, release.releaseId)}:history:${failed.intentId}`)!)).toMatchObject({ intent: { sourceTerminal: true, sourceStatus: 'reverted', sourceConfirmations: 2, sourceTxHash: SOURCE_HASH } })
    expect(acquireSponsoredOperationBrowserLane).toHaveBeenCalledTimes(1)
    expect(sendFundingTransactions).not.toHaveBeenCalled()
    expect(api.createIntent).not.toHaveBeenCalled()
  })

  it('preserves the active transfer when fresh source evidence retracts the terminal failure', async () => {
    const failed = terminalSourceIntentFixture()
    saveFunding(localStorage, savedFixture(failed))
    const api = apiFixture()
    api.intent.mockResolvedValueOnce(failed).mockResolvedValueOnce(intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH, sourceStatus: 'pending', sourceTerminal: false, sourceConfirmations: 0 }))
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    await waitFor(() => expect(screen.getByRole('button', { name: 'Archive failed transfer & start new quote' })).toBeInTheDocument())
    fireEvent.click(screen.getByRole('button', { name: 'Archive failed transfer & start new quote' }))
    await waitFor(() => expect(screen.getByRole('alert')).toBeInTheDocument())
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.intent.intentId).toBe(failed.intentId)
    expect(localStorage.getItem(`${fundingStorageKey(OWNER, release.releaseId)}:history:${failed.intentId}`)).toBeNull()
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.intent.sourceTerminal).toBe(false)
    expect(screen.queryByRole('button', { name: 'Archive failed transfer & start new quote' })).not.toBeInTheDocument()
    expect(screen.queryByRole('textbox', { name: 'Amount (USDC)' })).not.toBeInTheDocument()
    expect(sendFundingTransactions).not.toHaveBeenCalled()
    expect(api.createIntent).not.toHaveBeenCalled()
  })

  it.each([
    { sourceStatus: 'reverted' as const, sourceTerminal: false, sourceConfirmations: 1 },
    { bridgeStatus: 'refunded' as const },
  ])('keeps advisory source failure or provider refund status from unlocking another transfer', async (advisory) => {
    const intent = intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH, ...advisory })
    saveFunding(localStorage, savedFixture(intent))
    const api = apiFixture()
    api.intent.mockResolvedValue(intent)
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    await act(async () => {})
    expect(screen.queryByRole('button', { name: 'Archive failed transfer & start new quote' })).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Add more funds' })).not.toBeInTheDocument()
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
  })

  it('returns the owning wallet to Arbitrum and refreshes the destination identity', async () => {
    saveFunding(localStorage, savedFixture(intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH })))
    const api = apiFixture()
    api.intent.mockResolvedValue(intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH }))
    const request = vi.fn<FundingWallet['request']>().mockResolvedValueOnce([OWNER]).mockResolvedValueOnce(null)
    const wallet = vi.fn<() => Promise<FundingWallet>>().mockResolvedValue({ request })
    const identity = identityFixture({ status: 'blocked', chainId: 1, accountAddress: undefined })
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identity} manifest={release} api={api} wallet={wallet} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    fireEvent.click(screen.getByRole('button', { name: 'Return to Arbitrum' }))
    await waitFor(() => expect(identity.reloadIdentity).toHaveBeenCalledTimes(1))
    expect(request).toHaveBeenNthCalledWith(1, { method: 'eth_accounts' })
    expect(request).toHaveBeenNthCalledWith(2, { method: 'wallet_switchEthereumChain', params: [{ chainId: '0xa4b1' }] })
    expect(sendFundingTransactions).not.toHaveBeenCalled()
  })

  it('does not switch or reload a different owner’s wallet during recovery', async () => {
    saveFunding(localStorage, savedFixture())
    const request = vi.fn<FundingWallet['request']>().mockResolvedValue([OTHER_ADDRESS])
    const wallet = vi.fn<() => Promise<FundingWallet>>().mockResolvedValue({ request })
    const identity = identityFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identity} manifest={release} api={apiFixture()} wallet={wallet} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    fireEvent.click(screen.getByRole('button', { name: 'Return to Arbitrum' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('Reconnect the wallet that owns this Trading Account'))
    expect(request).toHaveBeenCalledTimes(1)
    expect(identity.reloadIdentity).not.toHaveBeenCalled()
  })

  it('keeps a rejected recovery hash editable and only persists a source hash the API accepts', async () => {
    saveFunding(localStorage, { ...savedFixture(), sourceSubmissionPending: true })
    const api = apiFixture()
    api.sourceSubmitted.mockRejectedValueOnce(new Error('This transaction does not match the funding intent.'))
    const wallet = walletFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture({ status: 'blocked', accountAddress: undefined, chainId: 1 })} manifest={release} api={api} wallet={wallet} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    const input = screen.getByRole('textbox', { name: 'Source transaction hash' })
    const typo = `0x${'99'.repeat(32)}`
    fireEvent.change(input, { target: { value: typo } })
    fireEvent.click(screen.getByRole('button', { name: 'Track existing transfer' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('This transaction does not match'))
    expect(input).toBeEnabled()
    expect(input).toHaveValue(typo)
    expect(restoreFunding(localStorage, OWNER, release.releaseId)).toMatchObject({ sourceSubmissionPending: true })
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.sourceTxHash).toBeUndefined()

    fireEvent.change(input, { target: { value: SOURCE_HASH } })
    fireEvent.click(screen.getByRole('button', { name: 'Track existing transfer' }))
    await waitFor(() => expect(restoreFunding(localStorage, OWNER, release.releaseId)).toMatchObject({ sourceSubmissionPending: false, sourceTxHash: SOURCE_HASH }))
    expect(screen.getByRole('status')).toHaveTextContent('Checking saved transfer')
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
    api.intent.mockResolvedValue(intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH }))
    await waitFor(() => expect(screen.getByRole('button', { name: 'Refresh transfer status' })).toBeEnabled())
    fireEvent.click(screen.getByRole('button', { name: 'Refresh transfer status' }))
    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Transfer in progress'))
    expect(screen.queryByRole('textbox', { name: 'Source transaction hash' })).not.toBeInTheDocument()
    expect(wallet).not.toHaveBeenCalled()
    expect(sendFundingTransactions).not.toHaveBeenCalled()
  })

  it('replaces a pending source hash only when the API confirms the submitted replacement', async () => {
    const pending = intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH, sourceStatus: 'pending', sourceConfirmations: 0 })
    saveFunding(localStorage, { ...savedFixture(pending), sourceTxHash: SOURCE_HASH })
    const api = apiFixture()
    api.intent.mockResolvedValue(pending)
    api.sourceSubmitted.mockResolvedValueOnce(pending).mockResolvedValueOnce({ ...pending, sourceTxHash: BLOCK_HASH })
    const wallet = walletFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={wallet} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    const input = screen.getByRole('textbox', { name: 'Source transaction hash' })
    fireEvent.change(input, { target: { value: BLOCK_HASH } })
    fireEvent.click(screen.getByRole('button', { name: 'Track existing transfer' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('The backend did not accept the submitted source transaction.'))
    expect(input).toHaveValue(BLOCK_HASH)
    expect(input).toBeEnabled()
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.sourceTxHash).toBe(SOURCE_HASH)

    fireEvent.click(screen.getByRole('button', { name: 'Track existing transfer' }))
    await waitFor(() => expect(restoreFunding(localStorage, OWNER, release.releaseId)).toMatchObject({ sourceTxHash: BLOCK_HASH, intent: { sourceTxHash: BLOCK_HASH } }))
    expect(api.sourceSubmitted).toHaveBeenLastCalledWith('intent-test-1', BLOCK_HASH)
    expect(wallet).not.toHaveBeenCalled()
    expect(sendFundingTransactions).not.toHaveBeenCalled()
  })

  it('recovers a replacement after source registration failed before the backend learned the original hash', async () => {
    saveFunding(localStorage, savedFixture())
    const firstApi = apiFixture()
    firstApi.sourceSubmitted.mockRejectedValue(new Error('Source registration could not reach the backend.'))
    vi.mocked(sendFundingTransactions).mockImplementation(async ({ beforeBridge, onBridgeHash }) => {
      beforeBridge()
      expect(restoreFunding(localStorage, OWNER, release.releaseId)?.sourceSubmissionPending).toBe(true)
      onBridgeHash(SOURCE_HASH)
      return SOURCE_HASH
    })
    const firstView = render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={firstApi} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Awaiting source transaction'))
    fireEvent.click(screen.getByRole('button', { name: 'Continue in wallet' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('Source registration could not reach the backend.'))
    expect(firstApi.sourceSubmitted).toHaveBeenCalledWith('intent-test-1', SOURCE_HASH)
    const interrupted = restoreFunding(localStorage, OWNER, release.releaseId)!
    expect(interrupted).toMatchObject({ sourceTxHash: SOURCE_HASH, sourceSubmissionPending: false })
    expect(interrupted.intent.sourceTxHash).toBeUndefined()
    expect(interrupted.intent.sourceStatus).toBeUndefined()
    firstView.unmount()

    // The wallet replaces the original transaction while this page is closed.
    // The backend still knows only the unsent intent when the page is reopened.
    const recoveryApi = apiFixture()
    recoveryApi.sourceSubmitted
      .mockRejectedValueOnce(new Error('Replacement transaction is not visible on Ethereum yet.'))
      .mockResolvedValueOnce(intentFixture({ status: 'bridging', sourceTxHash: BLOCK_HASH, sourceStatus: 'pending', sourceConfirmations: 0 }))
    const recoveryWallet = walletFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture({ status: 'blocked', accountAddress: undefined, chainId: 1 })} manifest={release} api={recoveryApi} wallet={recoveryWallet} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    const input = screen.getByRole('textbox', { name: 'Source transaction hash' })
    fireEvent.change(input, { target: { value: BLOCK_HASH } })
    fireEvent.click(screen.getByRole('button', { name: 'Track existing transfer' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('Replacement transaction is not visible'))
    expect(input).toBeEnabled()
    expect(input).toHaveValue(BLOCK_HASH)
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.sourceTxHash).toBe(SOURCE_HASH)
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.intent.sourceTxHash).toBeUndefined()

    fireEvent.click(screen.getByRole('button', { name: 'Track existing transfer' }))
    await waitFor(() => expect(restoreFunding(localStorage, OWNER, release.releaseId)).toMatchObject({ sourceTxHash: BLOCK_HASH, sourceSubmissionPending: false, intent: { sourceTxHash: BLOCK_HASH, sourceStatus: 'pending' } }))
    expect(recoveryApi.sourceSubmitted).toHaveBeenNthCalledWith(1, 'intent-test-1', BLOCK_HASH)
    expect(recoveryApi.sourceSubmitted).toHaveBeenNthCalledWith(2, 'intent-test-1', BLOCK_HASH)
    expect(recoveryWallet).not.toHaveBeenCalled()
    expect(sendFundingTransactions).toHaveBeenCalledTimes(1)
    expect(recoveryApi.createIntent).not.toHaveBeenCalled()
  })

  it.each([{ sourceTxHash: SOURCE_HASH }, { sourceSubmissionPending: true }])('does not resend when another tab records source activity before Continue is clicked', async (otherTabProgress) => {
    saveFunding(localStorage, savedFixture())
    const api = apiFixture()
    const wallet = walletFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={wallet} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Awaiting source transaction'))
    saveFunding(localStorage, { ...savedFixture(), ...otherTabProgress })
    fireEvent.click(screen.getByRole('button', { name: 'Continue in wallet' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('This transfer has already started'))
    expect(sendFundingTransactions).not.toHaveBeenCalled()
    expect(wallet).not.toHaveBeenCalled()
    expect(api.createIntent).not.toHaveBeenCalled()
    expect(acquireSponsoredOperationBrowserLane).toHaveBeenCalledWith({ chainId: 42161, accountAddress: OWNER, lane: `funding:${release.releaseId}` })
  })

  it('does not let a stale background poll erase another tab’s interrupted wallet request', async () => {
    saveFunding(localStorage, savedFixture())
    const pending = deferred<FundingIntent>()
    const api = apiFixture()
    api.intent.mockReturnValue(pending.promise)
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    expect(screen.getByRole('button', { name: 'Continue in wallet' })).toBeInTheDocument()

    saveFunding(localStorage, { ...savedFixture(), sourceSubmissionPending: true })
    await act(async () => { pending.resolve(intentFixture()) })
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.sourceSubmissionPending).toBe(true)
    expect(screen.getByRole('textbox', { name: 'Source transaction hash' })).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Continue in wallet' })).not.toBeInTheDocument()
    expect(sendFundingTransactions).not.toHaveBeenCalled()
  })

  it('rechecks whether funding is enabled before continuing a saved unsent intent', async () => {
    saveFunding(localStorage, savedFixture())
    const api = apiFixture()
    api.config.mockResolvedValue({ enabled: false, reason: 'New funding is temporarily disabled.' })
    const wallet = walletFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={wallet} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Awaiting source transaction'))
    fireEvent.click(screen.getByRole('button', { name: 'Continue in wallet' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('New funding is temporarily disabled.'))
    expect(api.config).toHaveBeenCalledTimes(1)
    expect(wallet).not.toHaveBeenCalled()
    expect(sendFundingTransactions).not.toHaveBeenCalled()
    expect(api.createIntent).not.toHaveBeenCalled()
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.intent.intentId).toBe('intent-test-1')
  })

  it('blocks new funding after malformed saved state rather than forgetting a possible transfer', async () => {
    localStorage.setItem(fundingStorageKey(OWNER, release.releaseId), '{invalid')
    const api = apiFixture()
    const wallet = walletFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={wallet} />)
    fireEvent.click(screen.getByRole('button', { name: 'Add funds' }))
    expect(screen.getByRole('alert')).toHaveTextContent('Saved funding could not be read')
    expect(screen.getByRole('textbox', { name: 'Amount (USDC)' })).toBeDisabled()
    fireEvent.click(screen.getByRole('button', { name: 'Review funding quote' }))
    await act(async () => {})
    expect(api.quote).not.toHaveBeenCalled()
    expect(api.createIntent).not.toHaveBeenCalled()
    expect(wallet).not.toHaveBeenCalled()
    expect(localStorage.getItem(fundingStorageKey(OWNER, release.releaseId))).toBe('{invalid')
  })
})

describe('reviewing a new funding quote', () => {
  it.each(['new quote', 'saved intent'])('refuses wallet activity for a %s when deployment verification fails', async (mode) => {
    const api = apiFixture()
    const wallet = walletFixture()
    verifyDeployment.mockRejectedValue(new Error('Destination handler code does not match the reviewed release.'))
    if (mode === 'saved intent') saveFunding(localStorage, savedFixture())
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={wallet} />)
    if (mode === 'saved intent') {
      fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
      await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Awaiting source transaction'))
      fireEvent.click(screen.getByRole('button', { name: 'Continue in wallet' }))
    } else {
      fireEvent.click(screen.getByRole('button', { name: 'Add funds' }))
      fireEvent.change(screen.getByRole('textbox', { name: 'Amount (USDC)' }), { target: { value: '100' } })
      fireEvent.click(screen.getByRole('button', { name: 'Review funding quote' }))
    }
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('Destination handler code does not match'))
    expect(verifyDeployment).toHaveBeenCalledWith(expect.objectContaining({ quoteId: QUOTE_ID, multicallHandler: MULTICALL_HANDLER, destinationSpokePool: DESTINATION_SPOKE_POOL }), expect.objectContaining({ owner: OWNER, beneficiary: BENEFICIARY, multicallHandler: MULTICALL_HANDLER, destinationSpokePool: DESTINATION_SPOKE_POOL }))
    expect(screen.queryByRole('button', { name: 'Approve & send from wallet' })).not.toBeInTheDocument()
    expect(api.createIntent).not.toHaveBeenCalled()
    expect(wallet).not.toHaveBeenCalled()
    expect(sendFundingTransactions).not.toHaveBeenCalled()
  })

  it('locks the amount and source asset while retrieving the quote the user will review', async () => {
    const pending = deferred<FundingQuote>()
    const api = apiFixture()
    api.quote.mockReturnValue(pending.promise)
    const wallet = walletFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={wallet} />)
    fireEvent.click(screen.getByRole('button', { name: 'Add funds' }))
    const amount = screen.getByRole('textbox', { name: 'Amount (USDC)' })
    const source = screen.getByRole('combobox', { name: 'Source asset' })
    fireEvent.change(amount, { target: { value: '100' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review funding quote' }))
    expect(amount).toBeDisabled()
    expect(source).toBeDisabled()
    await waitFor(() => expect(api.quote).toHaveBeenCalledWith({ ownerAddress: OWNER, beneficiary: BENEFICIARY, sourceChainId: 1, sourceToken: release.sources[0].token, sourceAmount: '100000000' }))
    expect(api.createIntent).not.toHaveBeenCalled()
    expect(wallet).not.toHaveBeenCalled()

    await act(async () => { pending.resolve(quoteFixture()) })
    expect(amount).toBeEnabled()
    expect(source).toBeEnabled()
    expect(screen.getByRole('button', { name: 'Approve & send from wallet' })).toBeEnabled()
    expect(prepareFundingTransactions).toHaveBeenCalledTimes(1)
    expect(screen.getByText(/Estimated arrival: 99 USDC/)).toBeInTheDocument()
  })

  it('requires the reviewed destination identity before starting a new source-chain quote', async () => {
    const api = apiFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture({ status: 'blocked', chainId: 1, accountAddress: undefined })} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'Add funds' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'Amount (USDC)' }), { target: { value: '100' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review funding quote' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('Return to your reviewed destination Trading Account'))
    expect(api.quote).not.toHaveBeenCalled()
    expect(api.createIntent).not.toHaveBeenCalled()
    expect(sendFundingTransactions).not.toHaveBeenCalled()
  })
})

describe('canonical return to the Trading Account', () => {
  it('shows historical returned funds without treating them as margin or a current balance', async () => {
    const returned = needsDepositIntentFixture()
    saveFunding(localStorage, savedFixture(returned))
    const pending = deferred<FundingIntent>()
    const api = apiFixture()
    api.intent.mockReturnValue(pending.promise)
    const onDepositAccount = vi.fn()
    const refresh = vi.fn()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} onDepositAccount={onDepositAccount} onAccountRefresh={refresh} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    expect(screen.queryByRole('button', { name: 'Review Trading Account deposit' })).not.toBeInTheDocument()
    await act(async () => { pending.resolve(returned) })
    expect(screen.getByRole('status')).toHaveTextContent('USDC returned — deposit needed')
    expect(screen.getByText(/99 USDC was returned/)).toHaveTextContent('may since have been spent or deposited')
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
    expect(refresh).toHaveBeenCalledTimes(1)
    expect(onDepositAccount).not.toHaveBeenCalled()
  })

  it('refreshes live account data before opening an explicitly pinned account deposit', async () => {
    const returned = needsDepositIntentFixture()
    saveFunding(localStorage, savedFixture(returned))
    const api = apiFixture()
    api.intent.mockResolvedValue(returned)
    const refreshed = deferred<void>()
    const refresh = vi.fn<() => Promise<void>>().mockResolvedValueOnce(undefined).mockReturnValueOnce(refreshed.promise)
    const onDepositAccount = vi.fn<(destination: FundingAccountDepositDestination) => void>()
    const wallet = walletFixture()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={wallet} onDepositAccount={onDepositAccount} onAccountRefresh={refresh} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    fireEvent.click(await screen.findByRole('button', { name: 'Review Trading Account deposit' }))
    expect(refresh).toHaveBeenCalledTimes(2)
    expect(onDepositAccount).not.toHaveBeenCalled()
    await act(async () => { refreshed.resolve() })
    expect(onDepositAccount).toHaveBeenCalledWith(expect.objectContaining({ owner: OWNER, beneficiary: BENEFICIARY, destinationChainId: 42161, token: release.token, clearinghouse: release.clearinghouse, releaseId: release.releaseId }))
    expect(onDepositAccount.mock.calls[0][0]).not.toHaveProperty('fallbackAmount')
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
    expect(wallet).not.toHaveBeenCalled()
    expect(sendFundingTransactions).not.toHaveBeenCalled()
  })

  it.each(['ownerAddress', 'accountAddress'] as const)('requires the original %s before opening a returned-funds deposit', async (field) => {
    const returned = needsDepositIntentFixture()
    saveFunding(localStorage, savedFixture(returned))
    const api = apiFixture()
    api.intent.mockResolvedValue(returned)
    const onDepositAccount = vi.fn()
    const refresh = vi.fn()
    const view = render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} onDepositAccount={onDepositAccount} onAccountRefresh={refresh} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    await screen.findByRole('button', { name: 'Review Trading Account deposit' })
    refresh.mockClear()
    view.rerender(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture({ [field]: OTHER_ADDRESS })} manifest={release} api={api} wallet={walletFixture()} onDepositAccount={onDepositAccount} onAccountRefresh={refresh} />)
    fireEvent.click(screen.getByRole('button', { name: 'Review Trading Account deposit' }))
    await waitFor(() => expect(screen.getByRole('alert')).toHaveTextContent('Return to the original Trading Account'))
    expect(onDepositAccount).not.toHaveBeenCalled()
    expect(refresh).not.toHaveBeenCalled()
  })

  it('requires fresh canonical return evidence before archiving without claiming margin credit', async () => {
    const returned = needsDepositIntentFixture()
    saveFunding(localStorage, savedFixture(returned))
    const finalRead = deferred<FundingIntent>()
    const api = apiFixture()
    api.intent.mockResolvedValueOnce(returned).mockReturnValueOnce(finalRead.promise)
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    fireEvent.click(await screen.findByRole('button', { name: 'Archive returned transfer & start new quote' }))
    await waitFor(() => expect(api.intent).toHaveBeenCalledTimes(2))
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.intent.intentId).toBe(returned.intentId)
    await act(async () => { finalRead.resolve(returned) })
    expect(screen.getByRole('textbox', { name: 'Amount (USDC)' })).toHaveValue('')
    expect(restoreFunding(localStorage, OWNER, release.releaseId)).toBeNull()
    expect(JSON.parse(localStorage.getItem(`${fundingStorageKey(OWNER, release.releaseId)}:history:${returned.intentId}`)!)).toMatchObject({ intent: { status: 'needs-deposit', fallbackAmount: '99000000', fallbackTxHash: returned.fallbackTxHash } })
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
    expect(sendFundingTransactions).not.toHaveBeenCalled()
  })

  it('preserves a returned transfer when a fresh read retracts its canonical evidence', async () => {
    const returned = needsDepositIntentFixture()
    saveFunding(localStorage, savedFixture(returned))
    const api = apiFixture()
    api.intent.mockResolvedValueOnce(returned).mockResolvedValueOnce(intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH }))
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    fireEvent.click(await screen.findByRole('button', { name: 'Archive returned transfer & start new quote' }))
    await screen.findByRole('alert')
    expect(restoreFunding(localStorage, OWNER, release.releaseId)).toMatchObject({ intent: { status: 'bridging' } })
    expect(localStorage.getItem(`${fundingStorageKey(OWNER, release.releaseId)}:history:${returned.intentId}`)).toBeNull()
    expect(screen.queryByRole('button', { name: 'Archive returned transfer & start new quote' })).not.toBeInTheDocument()
    expect(screen.queryByRole('textbox', { name: 'Amount (USDC)' })).not.toBeInTheDocument()
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
  })
})

describe('overlapping canonical funding reads', () => {
  it('does not restore readiness from an old confirmed poll after a newer reorg read', async () => {
    saveFunding(localStorage, savedFixture(confirmedIntentFixture()))
    const firstRead = deferred<FundingIntent>()
    const api = apiFixture()
    api.intent.mockReturnValueOnce(firstRead.promise).mockResolvedValueOnce(intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH }))
    const onAccountRefresh = vi.fn()
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} onAccountRefresh={onAccountRefresh} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    expect(api.intent).toHaveBeenCalledTimes(1)
    fireEvent.click(screen.getByRole('button', { name: 'Refresh transfer status' }))
    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Transfer in progress'))
    expect(api.intent).toHaveBeenCalledTimes(2)

    await act(async () => { firstRead.resolve(confirmedIntentFixture()) })
    expect(screen.getByRole('status')).toHaveTextContent('Transfer in progress')
    expect(screen.queryByText('Ready to trade')).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Add more funds' })).not.toBeInTheDocument()
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.intent.status).toBe('bridging')
    expect(onAccountRefresh).not.toHaveBeenCalled()
  })

  it('does not clear fresh readiness when an older canonical read fails late', async () => {
    saveFunding(localStorage, savedFixture(confirmedIntentFixture()))
    let rejectOldRead!: (error: Error) => void
    const oldRead = new Promise<FundingIntent>((_resolve, reject) => { rejectOldRead = reject })
    const api = apiFixture()
    api.intent.mockReturnValueOnce(oldRead).mockResolvedValueOnce(confirmedIntentFixture())
    render(<FundingFlow verifyDeployment={verifyDeployment} identity={identityFixture()} manifest={release} api={api} wallet={walletFixture()} />)
    fireEvent.click(screen.getByRole('button', { name: 'View funding transfer' }))
    fireEvent.click(screen.getByRole('button', { name: 'Refresh transfer status' }))
    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Ready to trade'))

    await act(async () => { rejectOldRead(new Error('The superseded read timed out.')) })
    expect(screen.getByRole('status')).toHaveTextContent('Ready to trade')
    expect(screen.queryByRole('alert')).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Add more funds' })).toBeEnabled()
    expect(restoreFunding(localStorage, OWNER, release.releaseId)?.intent.status).toBe('confirmed')
  })
})
