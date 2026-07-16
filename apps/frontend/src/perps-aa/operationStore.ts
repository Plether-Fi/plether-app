import type {
  PerpsActionKind,
  SponsoredExecutionStatus,
} from '@plether/perps-aa-client'
import type { Address, Hex } from 'viem'
import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import type {
  StableSponsorReason,
  UserOperationTerminalStatus,
} from './errors'

export type SponsoredOperationStatus =
  | SponsoredExecutionStatus
  | 'failed'
  | 'cancelled'
  | UserOperationTerminalStatus

export interface SponsoredOperation {
  id: string
  ownerAddress: Address
  accountAddress: Address
  chainId: number
  accountMode: string
  manifestVersion: string
  action: PerpsActionKind
  authorizationToken?: Address
  lane: string
  status: SponsoredOperationStatus
  sponsorshipAccepted: boolean
  userOperationHash?: Hex
  transactionHash?: Hex
  reason?: StableSponsorReason
  retryable?: boolean
  replacementUserOperationHash?: Hex
  retryCount: number
  createdAt: number
  updatedAt: number
  statusTimestamps: Partial<Record<SponsoredOperationStatus, number>>
}

export class SponsoredOperationLockedError extends Error {
  readonly operationId: string

  constructor(operationId: string) {
    super('Another Trading Account action is already being submitted')
    this.name = 'SponsoredOperationLockedError'
    this.operationId = operationId
  }
}

interface BeginSponsoredOperationInput {
  id: string
  ownerAddress: Address
  accountAddress: Address
  chainId: number
  accountMode: string
  manifestVersion: string
  action: PerpsActionKind
  authorizationToken?: Address
  lane?: string
}

interface SponsoredOperationState {
  operations: SponsoredOperation[]
  activeLanes: Record<string, string>

  beginOperation: (input: BeginSponsoredOperationInput) => void
  transition: (id: string, status: SponsoredOperationStatus) => void
  recordUserOperationHash: (id: string, hash: Hex) => void
  recordTransactionHash: (id: string, hash: Hex) => void
  incrementRetry: (id: string) => void
  failOperation: (input: {
    id: string
    status?: 'failed' | UserOperationTerminalStatus
    reason?: StableSponsorReason
    retryable: boolean
    replacementUserOperationHash?: Hex
  }) => void
  cancelOperation: (id: string) => void
  releaseLane: (id: string) => void
  cleanupOperations: () => void
  getActiveOperation: (accountAddress: Address, lane?: string) => SponsoredOperation | undefined
}

export const SPONSORED_OPERATION_STORAGE_NAME =
  'plether_perps_sponsored_operations'
export const DEFAULT_SPONSORED_OPERATION_LANE = 'default'

function laneKey(
  accountAddress: Address,
  lane = DEFAULT_SPONSORED_OPERATION_LANE
): string {
  return `${accountAddress.toLowerCase()}:${lane}`
}

function updateOperation(
  operations: SponsoredOperation[],
  id: string,
  update: (operation: SponsoredOperation) => SponsoredOperation
): SponsoredOperation[] {
  return operations.map((operation) => operation.id === id ? update(operation) : operation)
}

export function isSponsoredOperationTerminal(
  status: SponsoredOperationStatus
): boolean {
  return [
    'confirmed',
    'failed',
    'cancelled',
    'execution-reverted',
    'dropped',
    'replaced',
    'expired',
  ].includes(status)
}

export function canCancelSponsoredOperationLocally(
  operation: SponsoredOperation
): boolean {
  return operation.userOperationHash === undefined &&
    ![
      'submitting',
      'confirming',
      'confirmed',
      'failed',
      'cancelled',
      'execution-reverted',
      'dropped',
      'replaced',
      'expired',
      'receipt-timeout',
    ].includes(operation.status)
}

function releaseOperationLane(
  activeLanes: Record<string, string>,
  operationId: string
): Record<string, string> {
  return Object.fromEntries(
    Object.entries(activeLanes).filter(([, activeId]) => activeId !== operationId)
  )
}

export const useSponsoredOperationStore = create<SponsoredOperationState>()(
  devtools(
    persist(
      (set, get) => ({
        operations: [],
        activeLanes: {},

        beginOperation: (input) => {
          const lane = input.lane ?? DEFAULT_SPONSORED_OPERATION_LANE
          const key = laneKey(input.accountAddress, lane)
          const activeOperationId = get().activeLanes[key]
          if (activeOperationId) {
            throw new SponsoredOperationLockedError(activeOperationId)
          }

          const now = Date.now()
          const operation: SponsoredOperation = {
            ...input,
            lane,
            status: 'building',
            sponsorshipAccepted: false,
            retryCount: 0,
            createdAt: now,
            updatedAt: now,
            statusTimestamps: {
              building: now,
            },
          }

          set((state) => ({
            operations: [...state.operations, operation],
            activeLanes: {
              ...state.activeLanes,
              [key]: input.id,
            },
          }))
        },

        transition: (id, status) => {
          const now = Date.now()
          set((state) => ({
            operations: updateOperation(state.operations, id, (operation) => ({
              ...operation,
              status,
              sponsorshipAccepted:
                operation.sponsorshipAccepted ||
                status === 'estimating' ||
                status === 'requesting-sponsorship' ||
                status === 'awaiting-signature' ||
                status === 'submitting' ||
                status === 'confirming' ||
                status === 'confirmed',
              updatedAt: now,
              statusTimestamps: {
                ...operation.statusTimestamps,
                [status]: now,
              },
            })),
            ...(isSponsoredOperationTerminal(status)
              ? { activeLanes: releaseOperationLane(state.activeLanes, id) }
              : {}),
          }))
        },

        recordUserOperationHash: (id, hash) => {
          set((state) => ({
            operations: updateOperation(state.operations, id, (operation) => ({
              ...operation,
              userOperationHash: hash,
              updatedAt: Date.now(),
            })),
          }))
        },

        recordTransactionHash: (id, hash) => {
          set((state) => ({
            operations: updateOperation(state.operations, id, (operation) => ({
              ...operation,
              transactionHash: hash,
              updatedAt: Date.now(),
            })),
          }))
        },

        incrementRetry: (id) => {
          set((state) => ({
            operations: updateOperation(state.operations, id, (operation) => ({
              ...operation,
              retryCount: operation.retryCount + 1,
              updatedAt: Date.now(),
            })),
          }))
        },

        failOperation: ({
          id,
          status = 'failed',
          reason,
          retryable,
          replacementUserOperationHash,
        }) => {
          const now = Date.now()
          set((state) => ({
            operations: updateOperation(state.operations, id, (operation) => ({
              ...operation,
              status,
              reason,
              retryable,
              replacementUserOperationHash,
              updatedAt: now,
              statusTimestamps: {
                ...operation.statusTimestamps,
                [status]: now,
              },
            })),
            ...(isSponsoredOperationTerminal(status)
              ? { activeLanes: releaseOperationLane(state.activeLanes, id) }
              : {}),
          }))
        },

        cancelOperation: (id) => {
          const operation = get().operations.find((item) => item.id === id)
          if (!operation || !canCancelSponsoredOperationLocally(operation)) {
            return
          }
          get().transition(id, 'cancelled')
        },

        releaseLane: (id) => {
          set((state) => ({
            activeLanes: releaseOperationLane(state.activeLanes, id),
          }))
        },

        cleanupOperations: () => {
          const terminalCutoff = Date.now() - 24 * 60 * 60 * 1000
          const pendingCutoff = Date.now() - 60 * 60 * 1000
          set((state) => {
            const operations = state.operations.filter((operation) => {
              if (isSponsoredOperationTerminal(operation.status)) {
                return operation.updatedAt > terminalCutoff
              }
              if (operation.userOperationHash !== undefined) {
                return true
              }
              return operation.updatedAt > pendingCutoff
            })
            const retainedActiveIds = new Set(
              operations
                .filter((operation) =>
                  !isSponsoredOperationTerminal(operation.status)
                )
                .map((operation) => operation.id)
            )
            return {
              operations,
              activeLanes: Object.fromEntries(
                Object.entries(state.activeLanes)
                  .filter(([, operationId]) =>
                    retainedActiveIds.has(operationId)
                  )
              ),
            }
          })
        },

        getActiveOperation: (
          accountAddress,
          lane = DEFAULT_SPONSORED_OPERATION_LANE
        ) => {
          const state = get()
          const id = state.activeLanes[laneKey(accountAddress, lane)]
          return state.operations.find((operation) => operation.id === id)
        },
      }),
      {
        name: SPONSORED_OPERATION_STORAGE_NAME,
        partialize: (state) => ({
          operations: state.operations,
          activeLanes: state.activeLanes,
        }),
      }
    ),
    { name: 'SponsoredOperationStore' }
  )
)

const operationAbortControllers = new Map<string, AbortController>()

export function createSponsoredOperationSignal(operationId: string): AbortSignal {
  const existing = operationAbortControllers.get(operationId)
  if (existing) return existing.signal

  const controller = new AbortController()
  operationAbortControllers.set(operationId, controller)
  return controller.signal
}

export function cancelSponsoredOperationRequest(operationId: string): void {
  const operation = useSponsoredOperationStore.getState().operations
    .find((item) => item.id === operationId)
  if (!operation || !canCancelSponsoredOperationLocally(operation)) {
    return
  }

  operationAbortControllers.get(operationId)?.abort(
    new DOMException('Sponsored operation request cancelled', 'AbortError')
  )
  operationAbortControllers.delete(operationId)
  useSponsoredOperationStore.getState().cancelOperation(operationId)
}

export function releaseSponsoredOperationSignal(operationId: string): void {
  operationAbortControllers.delete(operationId)
}

export function hasSponsoredOperationSignal(operationId: string): boolean {
  return operationAbortControllers.has(operationId)
}
