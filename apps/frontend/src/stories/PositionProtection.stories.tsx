import { useEffect, useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, within } from 'storybook/test'
import { PositionProtectionManager, ProtectionHistoryRow, type ProtectionManagementRequest } from '../components/PerpsProtectionPanel'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import type { PositionProtection } from '../contracts/positionProtection'
import type { PerpsPosition } from '../hooks/usePerpsAccount'
import type { ProtectionHistoryRecord, ProtectionHistoryEvent } from '../hooks/useProtectionHistory'
import { PROTECTION_EXECUTION_REASONS, type ProtectionExecutionReport, type ProtectionExecutionReason } from '../utils/protectionExecution'
import { PerpsIdentityContext, type PerpsIdentityContextValue } from '../perps-aa'

const account = '0x1111111111111111111111111111111111111111'
const hash = '0x' + 'ab'.repeat(32)
const identity: PerpsIdentityContextValue = {
  status: 'ready', ownerAddress: account, accountAddress: account, chainId: 421614, isAaManifestConfigured: false, sponsorshipEnabled: false,
  manifest: null, identity: null, proposedIdentity: null, changedIdentityFields: [], error: null, confirmIdentityAfterContinuityCheck: () => false, reloadIdentity: () => undefined,
}
const scenarioStatus = { empty: 0, pending: 1, active: 2, queued: 3, closed: 4, failed: 5, removed: 6, liquidated: 7, delayed: 8 } as const
interface PreviewProps {
  scenario: keyof typeof scenarioStatus
  direction: 'long' | 'short'
  commitsEnabled: boolean
  showPosition: boolean
  narrow: boolean
  executionState: 'automatic' | 'unavailable' | 'stale' | 'transaction-pending' | 'loading' | 'error' | ProtectionExecutionReason
  managementOutcome: 'success' | 'rejected' | 'pending'
  legs: 'both' | 'take-profit' | 'stop-loss'
}
function protectionFixture(scenario: PreviewProps['scenario'], direction: PreviewProps['direction'], legs: PreviewProps['legs'] = 'both'): PositionProtection | undefined {
  if (scenario === 'empty') return undefined
  const triggered = ['queued', 'delayed', 'closed'].includes(scenario)
  const terminal = ['closed', 'failed', 'removed', 'liquidated'].includes(scenario)
  const takeProfitTriggerPrice = legs === 'stop-loss' ? 0n : direction === 'long' ? 90_000_000n : 110_000_000n
  const stopLossTriggerPrice = legs === 'take-profit' ? 0n : direction === 'long' ? 105_000_000n : 95_000_000n
  return {
    protectionId: 7n, parentOrderId: 12n, linkedOrderId: triggered ? 19n : 0n,
    account, side: direction === 'long' ? 0 : 1, size: 2_000n * 10n ** 18n,
    takeProfitTriggerPrice, stopLossTriggerPrice,
    triggerBountyUsdc: triggered || terminal ? 0n : 200_000n,
    executionBountyUsdc: terminal || scenario === 'queued' ? 0n : 200_000n,
    armedAt: scenario === 'pending' || scenario === 'failed' ? 0n : 1700000000n,
    armedBlock: scenario === 'pending' || scenario === 'failed' ? 0n : 100n,
    triggerMarkPrice: triggered ? (stopLossTriggerPrice || takeProfitTriggerPrice) : 0n,
    triggerPublishTime: triggered ? 1700000100n : 0n, triggeredLeg: triggered ? (stopLossTriggerPrice ? 2 : 1) : 0,
    status: scenarioStatus[scenario],
  }
}
function historyRecord(protection: PositionProtection): ProtectionHistoryRecord {
  return {
    ...protection, protectionId: protection.protectionId.toString(), parentOrderId: protection.parentOrderId.toString(),
    linkedOrderId: protection.linkedOrderId.toString(), size: protection.size.toString(),
    takeProfitTriggerPrice: protection.takeProfitTriggerPrice.toString(), stopLossTriggerPrice: protection.stopLossTriggerPrice.toString(),
    triggerBountyUsdc: protection.triggerBountyUsdc.toString(), executionBountyUsdc: protection.executionBountyUsdc.toString(),
    armedAt: protection.armedAt.toString(), armedBlock: protection.armedBlock.toString(), triggerMarkPrice: protection.triggerMarkPrice.toString(),
    triggerPublishTime: protection.triggerPublishTime.toString(), statusName: '', triggeredLegName: '', updatedBlock: '120',
  }
}
function historyEvents(protection: PositionProtection, latestFailureReason = 2): ProtectionHistoryEvent[] {
  const event = (name: string, block: number, args: ProtectionHistoryEvent['args'] = {}): ProtectionHistoryEvent => ({
    event: name, args, blockNumber: String(block), logIndex: '1', blockHash: '0x' + block.toString(16).padStart(64, '0'), transactionHash: hash,
  })
  const events = [event('PositionProtectionCreated', 100)]
  if (protection.status !== 1 && protection.status !== 5) events.unshift(event('PositionProtectionArmed', 101))
  if ([3, 4, 8].includes(protection.status)) {
    events.unshift(event('PositionProtectionTriggered', 110, { leg: protection.triggeredLeg, linkedOrderId: '18' }))
    events.unshift(event('PositionProtectionCloseAttemptFailed', 111, { linkedOrderId: '18', reason: 2, relatched: true }))
    events.unshift(event('PositionProtectionCloseAttemptQueued', 115, { linkedOrderId: '19' }))
  }
  if (protection.status === 8) events.unshift(event('PositionProtectionCloseAttemptFailed', 120, { linkedOrderId: '19', reason: latestFailureReason, relatched: true }))
  if ([4, 5, 7].includes(protection.status)) events.unshift(event('PositionProtectionTerminal', 120, { status: protection.status }))
  if (protection.status === 6) events.unshift(event('PositionProtectionCancelled', 120))
  return events
}
function ProtectionPreviewState({ scenario, direction, commitsEnabled, showPosition, narrow, executionState, managementOutcome, legs }: PreviewProps) {
  const fixture = protectionFixture(scenario, direction, legs)
  const terminal = [4, 5, 6, 7].includes(scenarioStatus[scenario])
  const [protection, setProtection] = useState(() => terminal ? undefined : fixture)
  const [historical, setHistorical] = useState(() => fixture)
  const [checkedAt, setCheckedAt] = useState(Date.now)
  useEffect(() => {
    const timer = window.setInterval(() => { setCheckedAt(Date.now()) }, 5_000)
    return () => { window.clearInterval(timer) }
  }, [])
  const position: PerpsPosition = {
    exists: !['pending', 'closed', 'failed', 'liquidated'].includes(scenario), side: direction === 'long' ? 0 : 1, direction,
    size: 2_000n * 10n ** 18n, entryPrice: 101_000_000n, marginUsdc: 400_000_000n,
    unrealizedPnlUsdc: 20_000_000n, maintenanceMarginUsdc: 2_000_000n, liquidatable: false,
    estimatedNotionalUsdc: 2_000_000_000n, liquidationPrice: 120_000_000n, pendingCarryUsdc: 600_000n,
  }
  async function manage(request: ProtectionManagementRequest) {
    if (managementOutcome === 'rejected') throw new Error('Wallet request rejected. Your existing TP/SL is unchanged.')
    if (managementOutcome === 'pending') await new Promise<void>(() => undefined)
    if (request.action === 'cancel') {
      if (protection) setHistorical({ ...protection, status: 6, triggerBountyUsdc: 0n, executionBountyUsdc: 0n })
      setProtection(undefined)
      return
    }
    const base = protection ?? protectionFixture('active', direction, legs)
    if (base && request.params) {
      const updated = { ...base, ...request.params }
      setProtection(updated)
      setHistorical(updated)
    }
  }
  const reason: ProtectionExecutionReason = PROTECTION_EXECUTION_REASONS.includes(executionState as ProtectionExecutionReason)
    ? executionState as ProtectionExecutionReason : scenario === 'delayed' ? 'retry-ready' : 'monitoring'
  const executionReport: ProtectionExecutionReport = {
    receivedAt: checkedAt,
    observation: !protection || executionState === 'unavailable' ? null : {
      protectionId: protection.protectionId.toString(), account, linkedOrderId: protection.linkedOrderId.toString(),
      protectionStatus: protection.status, reason, checkedBlock: '120', checkedBlockHash: hash,
      checkedAt: new Date(checkedAt - (executionState === 'stale' ? 120_000 : 0)).toISOString(),
      ageSeconds: executionState === 'stale' ? 120 : 0,
      outcomeReason: scenario === 'delayed' ? (executionState === 'operator-required' ? 7 : 2) : undefined,
      transactionHash: executionState === 'transaction-pending' ? hash : null,
      transactionAction: executionState === 'transaction-pending' ? 'retry' : null,
    },
  }
  const content = <PositionProtectionManager
    protection={protection} position={position} accountAddress={account} rawMark={100_000_000n} cap={200_000_000n}
    configuration={{ enabled: commitsEnabled, triggerBountyUsdc: 200_000n, executionBountyUsdc: 200_000n }}
    pendingOrders={scenario === 'pending' ? 1 : 0} onManage={manage}
    executionReport={executionState === 'loading' ? undefined : executionReport} executionLoading={executionState === 'loading'} executionError={executionState === 'error'}
    onRefreshExecution={() => { setCheckedAt(Date.now()) }}
    history={historical ? <ProtectionHistoryRow key={historical.status} row={historyRecord(historical)}
      initiallyExpanded={!protection} events={historyEvents(historical, executionState === 'operator-required' ? 7 : 2)} /> : <p className="text-xs text-content-secondary">Your confirmed TP/SL updates will appear here.</p>}
  />
  return <PerpsIdentityContext.Provider value={identity}>
    <main className="min-h-screen bg-app-bg px-3 py-6 sm:p-8">
      <div className={narrow ? 'mx-auto max-w-[360px]' : 'mx-auto max-w-3xl'}>
        <p className="mb-5 text-xs text-content-secondary">Interactive preview · changes stay in this browser. No wallet transactions.</p>
        <PerpsAccountPanel
          initialTab={showPosition ? 'position' : 'protections'} position={position} positionProtection={protection}
          protectionCapPrice={200_000_000n} protectionContent={content} pendingOrders={[]} orderHistory={[]} tradeHistory={[]}
          isConnected equityUsdc={420_000_000n} freeBuyingPowerUsdc={1_200_000_000n}
        />
      </div>
    </main>
  </PerpsIdentityContext.Provider>
}
// Primitive controls; construct contract BigInts inside the preview.
function ProtectionPreview(props: PreviewProps) {
  return <ProtectionPreviewState key={Object.values(props).join(':')} {...props} />
}
const meta = {
  title: 'Perps/Position Protection', component: ProtectionPreview,
  parameters: { layout: 'fullscreen' },
  argTypes: {
    scenario: { control: 'select', options: Object.keys(scenarioStatus) },
    direction: { control: 'inline-radio', options: ['long', 'short'] },
    executionState: { control: 'select', options: ['automatic', ...PROTECTION_EXECUTION_REASONS, 'transaction-pending', 'unavailable', 'stale', 'loading', 'error'] },
    managementOutcome: { control: 'select', options: ['success', 'rejected', 'pending'] },
    legs: { control: 'inline-radio', options: ['both', 'take-profit', 'stop-loss'] },
  },
  args: { scenario: 'active', direction: 'long', commitsEnabled: true, showPosition: false, narrow: false, executionState: 'automatic', managementOutcome: 'success', legs: 'both' },
} satisfies Meta<typeof ProtectionPreview>
export default meta
type Story = StoryObj<typeof meta>
export const Armed: Story = { name: 'Active TP/SL' }
export const PositionOverview: Story = { args: { showPosition: true } }
export const NoTriggers: Story = { args: { scenario: 'empty' } }
export const PendingOpen: Story = { args: { scenario: 'pending' } }
export const CloseQueued: Story = { args: { scenario: 'queued' } }
export const Latched: Story = { name: 'Close delayed', args: { scenario: 'delayed', executionState: 'queue-congested' } }
export const Closed: Story = { name: 'Closed · completed TP/SL', args: { scenario: 'closed' } }
export const Failed: Story = { name: 'Failed · protection ended', args: { scenario: 'failed' } }
export const Removed: Story = { name: 'Removed · position remains open', args: { scenario: 'removed' } }
export const Liquidated: Story = { args: { scenario: 'liquidated' } }
export const LatchedOracleUnavailable: Story = { args: { scenario: 'delayed', executionState: 'oracle-unavailable' } }
export const LatchedPendingOrders: Story = { args: { scenario: 'delayed', executionState: 'pending-orders' } }
export const LatchedOperatorRequired: Story = { args: { scenario: 'delayed', executionState: 'operator-required' } }
export const LatchedExecutionPaused: Story = { args: { scenario: 'delayed', executionState: 'execution-disabled' } }
export const LatchedRetryReady: Story = { args: { scenario: 'delayed', executionState: 'retry-ready' } }
export const LatchedRetryPending: Story = { args: { scenario: 'delayed', executionState: 'transaction-pending' } }
export const LatchedQueueCleanup: Story = { args: { scenario: 'delayed', executionState: 'queue-cleanup' } }
export const LatchedCheckFailed: Story = { args: { scenario: 'delayed', executionState: 'check-failed' } }
export const LatchedStaleStatus: Story = { args: { scenario: 'delayed', executionState: 'stale' } }
export const LatchedStatusUnavailable: Story = { args: { scenario: 'delayed', executionState: 'unavailable' } }
export const ExecutionStatusLoading: Story = { args: { scenario: 'delayed', executionState: 'loading' } }
export const ExecutionStatusError: Story = { args: { scenario: 'delayed', executionState: 'error' } }
export const ArmedOracleFrozen: Story = { args: { executionState: 'oracle-frozen' } }
export const ArmedOracleUnavailable: Story = { args: { executionState: 'oracle-unavailable' } }
export const CommitsDisabled: Story = { args: { commitsEnabled: false } }
export const ShortPosition: Story = { args: { direction: 'short' } }
export const TakeProfitOnly: Story = { args: { legs: 'take-profit' } }
export const StopLossOnly: Story = { args: { legs: 'stop-loss' } }
export const Inputs: Story = {
  name: 'Edit TP/SL',
  play: async ({ canvasElement }) => {
    await userEvent.click(within(canvasElement).getByRole('button', { name: 'Edit TP/SL' }))
  },
}
export const Review: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await userEvent.click(canvas.getByRole('button', { name: 'Edit TP/SL' }))
    const takeProfit = canvas.getByLabelText('Take profit (USDC)')
    await userEvent.clear(takeProfit)
    await userEvent.type(takeProfit, '1.12')
    await userEvent.click(canvas.getByRole('button', { name: 'Review TP/SL' }))
    await expect(canvas.getByRole('heading', { name: 'Review your TP/SL' })).toBeVisible()
    await expect(canvas.getByText('1.1200')).toBeVisible()
  },
}
export const NarrowEditor: Story = { ...Inputs, args: { narrow: true } }
export const NarrowDelayedClose: Story = { args: { narrow: true, scenario: 'delayed', executionState: 'operator-required' } }
export const RemoveConfirmation: Story = {
  play: async ({ canvasElement }) => { await userEvent.click(within(canvasElement).getByRole('button', { name: 'Remove TP/SL' })) },
}
export const WalletPending: Story = {
  args: { managementOutcome: 'pending' },
  play: async context => {
    await Review.play?.(context)
    await userEvent.click(within(context.canvasElement).getByRole('button', { name: 'Confirm TP/SL' }))
    await expect(within(context.canvasElement).getByText('Confirm in your wallet, then wait for the update.')).toBeVisible()
  },
}
export const WalletRejected: Story = {
  args: { managementOutcome: 'rejected' },
  play: async context => {
    await Review.play?.(context)
    await userEvent.click(within(context.canvasElement).getByRole('button', { name: 'Confirm TP/SL' }))
    await expect(within(context.canvasElement).getByRole('alert')).toHaveTextContent('Wallet request rejected')
  },
}
