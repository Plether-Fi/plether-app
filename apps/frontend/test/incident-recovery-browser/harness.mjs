// Browser boundary fixture: the execution, readiness, Web Locks, authorization
// storage and operation journal below are the production modules, not copies.
import { executeSponsoredPerpsAction, resumeSponsoredPerpsAction } from '../../src/perps-aa/execution.ts'
import { useSponsoredOperationStore, SPONSORED_OPERATION_JOURNAL_PREFIX } from '../../src/perps-aa/operationStore.ts'
import { getOrCreateDepositAuthorization } from '../../src/perps-aa/authorizationStore.ts'
import { currentReadiness, refreshReadiness } from '../../src/perps-aa/readiness.ts'
import { createElement } from 'react'
import { createRoot } from 'react-dom/client'
import { concatHex, numberToHex } from 'viem'
import { getUserOperationHash } from 'viem/account-abstraction'
import { PerpsAaRuntimeContext } from '../../src/perps-aa/runtimeContext.ts'
import { SponsoredOperationRecovery } from '../../src/perps-aa/SponsoredOperationRecovery.tsx'
import { PIMLICO_SINGLETON_PAYMASTER_V8 } from '../../src/perps-aa/paymasterValidity.ts'

const OWNER = '0x1111111111111111111111111111111111111111'
const ACCOUNT = '0x2222222222222222222222222222222222222222'
const TARGET = '0x3333333333333333333333333333333333333333'
const ENTRY_POINT = '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108'
const FACTORY = '0x13E9ed32155810FDbd067D4522C492D6f68E5944'
const PAYMASTER = '0x1234567890123456789012345678901234567890'
const manifest = {
  version: 'perps-aa-arbitrum-sepolia-v2', orderInterfaceVersion: 3,
  chainId: 421614, entryPoint: ENTRY_POINT, entryPointVersion: '0.8',
  bundlerRpcUrl: '/__incident-drill/no-live-bundler', paymasterRpcUrl: '/__incident-drill/prepare',
  preparationRpcVersion: 1, paymasterAddress: PAYMASTER, paymasterVersion: 'plether-verifying-v1',
  smartAccountMode: 'simple', smartAccountVersion: 'permissionless-simple-v0.8',
  smartAccountIndex: '0', smartAccountFactory: FACTORY, settlementToken: TARGET,
  settlementTokenSupportsEip3009: false, settlementTokenEip712Name: null, settlementTokenEip712Version: null,
  marginClearinghouse: TARGET, cfdEngine: TARGET, orderRouter: TARGET,
  orderLifecycleBook: TARGET, policyEvaluator: TARGET, userOperationExplorerUrlTemplate: null,
  transactionExplorerUrlTemplate: null, testnetFaucet: null, sponsorshipEnabled: true,
}
const action = { kind: 'deposit', account: ACCOUNT, calls: [{ to: TARGET, value: 0n, data: '0x1234' }] }
const boundaryCalls = { prepare: 0, sign: 0, send: 0, receipt: 0, status: 0, snapshot: 0 }
const SIGNED_FIXTURE_ID = 'incident-recovery-drill-signed-id'
let recoveryRoot

async function forbiddenCall(name) {
  boundaryCalls[name]++
  await fetch(`/__incident-drill/${name}`, { method: 'POST', body: '{}' })
  throw new Error(`The isolated drill never ${name}s an operation`)
}

const runtime = {
  chainId: 421614, ownerAddress: OWNER, factoryAddress: FACTORY,
  accountVersion: 'permissionless-simple-v0.8', accountIndex: '0', manifestVersion: manifest.version,
  async getRecoverySnapshot(userOperationHash) {
    boundaryCalls.snapshot++
    const response = await fetch(`/__incident-drill/recovery-snapshot?hash=${userOperationHash}`)
    const body = await response.json()
    return { ...body, blockNumber: BigInt(body.blockNumber), blockTimestamp: BigInt(body.blockTimestamp), accountNonce: BigInt(body.accountNonce) }
  },
  smartAccount: {
    accountAddress: ACCOUNT, entryPoint: ENTRY_POINT,
    async prepareUserOperation(request) {
      boundaryCalls.prepare++
      let response
      try {
        response = await fetch('/__incident-drill/prepare', {
          method: 'POST', headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ preparationId: request.preparationId, action: request.action }),
        })
      } catch {
        throw new Error('preparation response lost')
      }
      const body = await response.json()
      // A real loopback HTTP response is classified by production error helpers.
      throw Object.assign(new Error('Fixture preparation refused'), {
        cause: { data: { reason: body.reason, retryable: true } },
      })
    },
    signUserOperation: () => forbiddenCall('sign'),
    sendUserOperation: () => forbiddenCall('send'),
    getUserOperationHash(operation) {
      return getUserOperationHash({ userOperation: operation, entryPointAddress: ENTRY_POINT, entryPointVersion: '0.8', chainId: 421614 })
    },
    async getUserOperationStatus(userOperationHash) {
      boundaryCalls.status++
      const response = await fetch(`/__incident-drill/status?hash=${userOperationHash}`)
      return response.json()
    },
    async getUserOperationReceipt(userOperationHash) {
      boundaryCalls.receipt++
      const response = await fetch(`/__incident-drill/receipt?hash=${userOperationHash}`)
      const body = await response.json()
      if (body.kind === 'not-found') {
        const error = new Error('Fixture receipt not found')
        error.name = 'UserOperationReceiptNotFoundError'
        throw error
      }
      if (body.userOpHash !== userOperationHash) throw new Error('Fixture returned the wrong exact receipt')
      return {
        ...body, actualGasCost: BigInt(body.actualGasCost), actualGasUsed: BigInt(body.actualGasUsed), nonce: BigInt(body.nonce),
        receipt: { ...body.receipt, blockNumber: BigInt(body.receipt.blockNumber) },
      }
    },
  },
}

function authorizationRecords() {
  return Object.keys(localStorage).filter(key => key.startsWith('plether_perps_eip3009_v2:'))
    .sort().map(key => [key, localStorage.getItem(key)])
}

function mountSignedRecovery() {
  if (recoveryRoot) return
  const target = document.createElement('div')
  target.id = 'signed-recovery-coordinator'
  document.body.append(target)
  recoveryRoot = createRoot(target)
  recoveryRoot.render(createElement(PerpsAaRuntimeContext.Provider, { value: runtime }, createElement(SponsoredOperationRecovery)))
}

function signedSnapshot() {
  const id = sessionStorage.getItem(SIGNED_FIXTURE_ID)
  const state = useSponsoredOperationStore.getState()
  const operation = state.operations.find(value => value.id === id)
  return {
    operationCount: state.operations.length,
    operation: operation ? {
      id: operation.id, status: operation.status, reason: operation.reason, retryable: operation.retryable,
      userOperationHash: operation.userOperationHash, signedUserOperation: JSON.stringify(operation.signedUserOperation),
      authorizationNonce: operation.authorizationNonce,
      hasSignedOperation: operation.signedUserOperation !== undefined, submissionMetadataVersion: operation.submissionMetadataVersion,
      transactionHash: operation.transactionHash, journalPresent: localStorage.getItem(`${SPONSORED_OPERATION_JOURNAL_PREFIX}${operation.id}`) !== null,
    } : null,
    activeOperationId: state.getActiveOperation(ACCOUNT)?.id ?? null,
    authorizations: authorizationRecords(), boundaryCalls: { ...boundaryCalls },
  }
}

async function rejected(operation) {
  try { await operation(); return { unexpectedlySucceeded: true } }
  catch (error) { return { name: error.name, reason: error.reason, message: error.message } }
}

function snapshot() {
  const state = useSponsoredOperationStore.getState()
  const active = state.getActiveOperation(ACCOUNT)
  return {
    operationCount: state.operations.length,
    active: active ? {
      id: active.id, status: active.status, reason: active.reason,
      nativePreparation: JSON.stringify(active.nativePreparation),
      nativePreparationId: active.nativePreparation?.preparationId,
      hasUserOperationHash: active.userOperationHash !== undefined,
      hasSignedOperation: active.signedUserOperation !== undefined,
      journalPresent: localStorage.getItem(`${SPONSORED_OPERATION_JOURNAL_PREFIX}${active.id}`) !== null,
    } : null,
    authorizations: authorizationRecords(),
    readiness: currentReadiness(),
    actualBrowserLocksAvailable: typeof navigator.locks?.request === 'function',
    boundaryCalls: { ...boundaryCalls },
  }
}

await useSponsoredOperationStore.persist.rehydrate()
window.incidentRecoveryDrill = {
  snapshot,
  signedSnapshot,
  async seedSignedUnknown() {
    const store = useSponsoredOperationStore.getState()
    if (store.operations.length !== 0 || sessionStorage.getItem(SIGNED_FIXTURE_ID)) throw new Error('A signed fixture may be seeded only once in a fresh browser context')
    const id = crypto.randomUUID()
    const validUntil = BigInt(Math.floor(Date.now() / 1000) + 3600)
    // Deliberately invalid four-byte signature; it is never signed or submitted.
    const operation = {
      sender: ACCOUNT, nonce: 7n, callData: '0x1234', callGasLimit: 1n, verificationGasLimit: 2n,
      preVerificationGas: 3n, maxFeePerGas: 4n, maxPriorityFeePerGas: 5n,
      paymaster: PIMLICO_SINGLETON_PAYMASTER_V8,
      paymasterData: concatHex(['0x01', numberToHex(validUntil, { size: 6 }), numberToHex(0n, { size: 6 }), `0x${'11'.repeat(65)}`]),
      paymasterVerificationGasLimit: 6n, paymasterPostOpGasLimit: 7n, signature: '0xdeadbeef',
    }
    const authorization = getOrCreateDepositAuthorization({ chainId: 421614, ownerAddress: OWNER, accountAddress: ACCOUNT, token: TARGET, amount: 1n })
    const userOperationHash = runtime.smartAccount.getUserOperationHash(operation)
    store.beginOperation({ id, ownerAddress: OWNER, accountAddress: ACCOUNT, chainId: 421614, accountMode: 'simple', manifestVersion: manifest.version,
      action: 'deposit', authorizationToken: TARGET, authorizationNonce: authorization.nonce })
    if (!store.recordUserOperationHash(id, userOperationHash, { signedUserOperation: operation })) throw new Error('The synthetic signed journal was not persisted')
    store.failOperation({ id, status: 'submission-unknown', reason: 'SUBMISSION_OUTCOME_UNKNOWN', retryable: false })
    sessionStorage.setItem(SIGNED_FIXTURE_ID, id)
    return signedSnapshot()
  },
  mountSignedRecovery,
  async refresh() { await refreshReadiness(true); return snapshot() },
  async newAction(withAuthorization = false) {
    let authorization
    if (withAuthorization) authorization = getOrCreateDepositAuthorization({
      chainId: 421614, ownerAddress: OWNER, accountAddress: ACCOUNT, token: TARGET, amount: 1n,
    })
    return rejected(() => executeSponsoredPerpsAction({
      manifest, ownerAddress: OWNER, action, runtime,
      ...(authorization ? { authorizationTokenToClearOnConfirmation: TARGET, authorizationNonceToClearOnConfirmation: authorization.nonce } : {}),
    }))
  },
  async resume() {
    const active = useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT)
    if (!active) throw new Error('Expected the persisted uncertain operation')
    return rejected(() => resumeSponsoredPerpsAction(active, runtime))
  },
}
if (new URL(location.href).searchParams.get('recovery') === 'signed' && sessionStorage.getItem(SIGNED_FIXTURE_ID)) mountSignedRecovery()
document.querySelector('#status').textContent = 'Production execution and recovery modules loaded; loopback-only drill ready.'
