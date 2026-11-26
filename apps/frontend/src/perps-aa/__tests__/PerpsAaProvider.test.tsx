import { render, screen } from '@testing-library/react'
import type { ReactNode } from 'react'
import { describe, expect, it, vi } from 'vitest'
import { PerpsAaProvider } from '../PerpsAaProvider'
import { PerpsIdentityContext, type PerpsIdentityContextValue } from '../PerpsIdentityContext'
import { kernelSepoliaCanaryProfile as profile } from '../kernelDeploymentPins'
import type { KernelPerpsAaDeploymentManifest } from '../manifest'
import { usePerpsAaRuntime, type PerpsAaSmartAccountRuntime } from '../runtimeContext'

vi.mock('wagmi', () => ({ usePublicClient: () => undefined, useWalletClient: () => ({}) }))
vi.mock('../../wallet/useWalletActions', () => ({ useWalletActions: () => ({ configured: false }) }))
vi.mock('../PerpsIdentityProvider', () => ({
  WagmiPerpsIdentityProvider: ({ children }: { children: ReactNode }) => (
    <PerpsIdentityContext value={identity}>{children}</PerpsIdentityContext>
  ),
}))
vi.mock('../useKernelAttempts', () => ({
  KernelOperationRecovery: () => <span>Kernel recovery</span>,
}))

const owner = '0x0000000000000000000000000000000000000001'
const sender = '0x0000000000000000000000000000000000000002'
const other = '0x0000000000000000000000000000000000000003'
const manifest = {
  smartAccountMode: 'kernel',
  version: `perps-aa-kernel-v1:${profile.deployment}`,
  kernel: { profile },
} as KernelPerpsAaDeploymentManifest
const runtime = {
  chainId: profile.chainId,
  ownerAddress: owner,
  manifestVersion: manifest.version,
  smartAccount: { accountAddress: sender },
  kernel: { scope: { deployment: profile.deployment } },
} as PerpsAaSmartAccountRuntime
const baseIdentity: PerpsIdentityContextValue = {
  status: 'blocked',
  ownerAddress: owner,
  accountAddress: undefined,
  chainId: profile.chainId,
  isAaManifestConfigured: true,
  sponsorshipEnabled: false,
  manifest,
  identity: null,
  proposedIdentity: null,
  changedIdentityFields: [],
  error: null,
  confirmIdentityAfterContinuityCheck: () => false,
  reloadIdentity: () => {},
}
let identity: PerpsIdentityContextValue

function RuntimeConsumer() {
  return <span>{usePerpsAaRuntime()?.kernel ? 'Kernel ready' : 'Kernel unavailable'}</span>
}

describe('Kernel runtime publication', () => {
  it('requires a ready identity bound to the current runtime for consumers and recovery', async () => {
    const ui = () => (
      <PerpsAaProvider manifestUrl={null} kernelDeployment={profile.deployment} runtime={runtime}>
        <RuntimeConsumer />
      </PerpsAaProvider>
    )
    identity = baseIdentity
    const view = render(ui())
    expect(screen.getByText('Kernel unavailable')).toBeInTheDocument()
    expect(screen.queryByText('Kernel recovery')).not.toBeInTheDocument()

    identity = { ...identity, status: 'ready', accountAddress: other }
    view.rerender(ui())
    expect(screen.getByText('Kernel unavailable')).toBeInTheDocument()
    expect(screen.queryByText('Kernel recovery')).not.toBeInTheDocument()

    identity = { ...identity, accountAddress: sender }
    view.rerender(ui())
    expect(screen.getByText('Kernel ready')).toBeInTheDocument()
    expect(await screen.findByText('Kernel recovery')).toBeInTheDocument()

    identity = { ...identity, status: 'blocked' }
    view.rerender(ui())
    expect(screen.getByText('Kernel unavailable')).toBeInTheDocument()
    expect(screen.queryByText('Kernel recovery')).not.toBeInTheDocument()
  })
})
