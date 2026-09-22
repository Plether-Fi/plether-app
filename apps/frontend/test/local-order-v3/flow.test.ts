import { beforeAll, beforeEach, describe, expect, it, vi } from 'vitest'
import { concatHex, encodeFunctionData, numberToHex, parseAbiItem, parseEventLogs, type Address, type Hex } from 'viem'
import { getUserOperationHash, toPackedUserOperation } from 'viem/account-abstraction'
import { buildPlaceOrderV3Action, buildProtectedOpenAction } from '@plether-fi/perps-aa-client'
import { preparePerpsOrderV3 } from '../../src/contracts/preparePerpsOrderV3'
import { persistPerpsOrderRequestV3, type PerpsOrderRequestV3 } from '../../src/contracts/perpsOrderV3'
import { executeSponsoredPerpsAction } from '../../src/perps-aa/execution'
import { resolveProtocolOperation } from '../../src/perps-aa/protocolOperationResolution'
import { recoverCanonicalInclusion } from '../../src/perps-aa/canonicalRecovery'
import { useSponsoredOperationStore, restoreSponsoredOperationLane } from '../../src/perps-aa/operationStore'
import { PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH, pletherSponsorshipValidUntil } from '../../src/perps-aa/paymasterValidity'
import type { PerpsAaDeploymentManifestV2 } from '../../src/perps-aa/manifest'
import type { ManagedUserOperation, ManagedUserOperationReceipt, PerpsAaSmartAccountRuntime } from '../../src/perps-aa/runtimeContext'
import { client, createStack, read, write, rpc, warp, now, owner, sponsor, keeper, policyDeadline } from './chain'

// Substitute local addresses and browser/telemetry transport only. The review,
// encoders, journal, signatures, paymaster, EntryPoint, router and recovery are real.
const registry = vi.hoisted(() => ({ addresses: {} as Record<string, Address> }))
vi.mock('../../src/contracts/perpsAddresses', () => ({ PERPS_ARBITRUM_SEPOLIA: registry.addresses }))
vi.mock('../../src/analytics/perps', () => ({ trackPerpsSponsoredOperation: vi.fn() }))
vi.mock('../../src/perps-aa/attemptDiagnostics', () => ({ reportAttemptStage: vi.fn() }))
vi.mock('../../src/perps-aa/recoveryDiagnostics', () => ({ reportRecoveryDiagnostic: vi.fn() }))
vi.mock('../../src/analytics/readiness', () => ({ reportReadiness: vi.fn() }))
const event = parseAbiItem('event UserOperationEvent(bytes32 indexed userOpHash, address indexed sender, address indexed paymaster, uint256 nonce, bool success, uint256 actualGasCost, uint256 actualGasUsed)')
let stack: Awaited<ReturnType<typeof createStack>>
let manifest: PerpsAaDeploymentManifestV2
let snapshot: Hex
const realFetch = globalThis.fetch
beforeAll(async () => {
  stack = await createStack()
  expect(stack.accountHash).toBe(PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH)
  Object.assign(registry.addresses, { housePool: stack.pool.address, perpsPublicLens: stack.lens.address,
    cfdEngineLens: stack.engineLens.address, cfdEngineAccountLens: stack.accountLens.address,
    pletherOracle: stack.oracle.address, cfdEngine: stack.engine.address, orderRouter: stack.router.address,
    marginClearinghouse: stack.clearinghouse.address, positionProtectionBook: stack.protection.address })
  manifest = { version: 'perps-aa-arbitrum-sepolia-v2', orderInterfaceVersion: 3, chainId: 421614,
    smartAccountMode: 'simple', entryPoint: stack.entryPoint.address, entryPointVersion: '0.8',
    smartAccountFactory: stack.factory.address, smartAccountVersion: 'permissionless-simple-v0.8', smartAccountIndex: '0',
    bundlerRpcUrl: '/local-aa', paymasterRpcUrl: '/local-aa', paymasterAddress: stack.paymaster.address, paymasterVersion: 'plether-verifying-v1', preparationRpcVersion: 1,
    usdc: stack.token.address, usdcSupportsEip3009: false, usdcEip712Name: null, usdcEip712Version: null, marginClearinghouse: stack.clearinghouse.address, cfdEngine: stack.engine.address,
    orderRouter: stack.router.address, orderLifecycleBook: stack.book.address, policyEvaluator: stack.evaluator.address,
    positionProtectionBook: stack.protection.address, testnetFaucet: null, sponsorshipEnabled: true,
    userOperationExplorerUrlTemplate: 'http://localhost/{userOperationHash}', transactionExplorerUrlTemplate: 'http://localhost/{transactionHash}',
  }
  Object.defineProperty(navigator, 'locks', { configurable: true, value: {
    request: async (name: string, _options: unknown, callback: (lock: unknown) => Promise<unknown>) => callback({ name }),
  } })
  vi.stubGlobal('fetch', async (input: RequestInfo | URL, init?: RequestInit) => {
    if (input === '/api/perps/v1/readiness') {
      const timestamp = Number(await now()) * 1000
      return new Response(JSON.stringify({ version: 1, observedAt: timestamp, expiresAt: timestamp + 15000, enforcementEnabled: true,
        actions: Object.fromEntries(['deposit', 'open', 'close', 'protection'].map(action => [action, [{ component: 'local_chain', status: 'ready', reason: 'READY' }]])),
      }), { headers: { Date: new Date(timestamp).toUTCString(), 'Content-Type': 'application/json' } })
    }
    return realFetch(input, init)
  })
  snapshot = await rpc('evm_snapshot') as Hex
})
beforeEach(async () => {
  await rpc('evm_revert', [snapshot]); snapshot = await rpc('evm_snapshot') as Hex
  localStorage.clear(); useSponsoredOperationStore.setState({ operations: [] })
  await stack.mark()
})
async function review() {
  return (await preparePerpsOrderV3(client, manifest, { account: stack.account.address, direction: 'long', side: 0,
    sizeDelta: 5000n * 10n ** 18n, marginDelta: 1000_000_000n, slippagePercent: 1, isClose: false, selectedMaxLeverageBps: 60_000 })).request
}
function runtime(request: PerpsOrderRequestV3, options: { delay?: bigint; lostResponse?: boolean; commitAt?: bigint } = {}) {
  let receipt: ManagedUserOperationReceipt
  const hash = (operation: ManagedUserOperation) => getUserOperationHash({ userOperation: operation, chainId: 421614, entryPointAddress: stack.entryPoint.address, entryPointVersion: '0.8' })
  const managed: PerpsAaSmartAccountRuntime = { chainId: 421614, ownerAddress: owner.address, factoryAddress: stack.factory.address,
    accountVersion: 'permissionless-simple-v0.8', accountIndex: '0',
    sponsorshipValidUntil: operation => pletherSponsorshipValidUntil(stack.paymaster.address, operation),
    getRecoverySnapshot: async (operationHash, _nonceKey, context) => {
      const safe = await client.getBlock({ blockTag: 'safe' })
      const logs = await client.getLogs({ address: stack.entryPoint.address, event, args: { userOpHash: operationHash }, fromBlock: 0n, toBlock: safe.number })
      const located = context?.transactionHash ?? logs[0]?.transactionHash
      return { blockNumber: safe.number, blockTimestamp: safe.timestamp,
        accountNonce: await client.readContract({ ...stack.entryPoint, functionName: 'getNonce', args: [stack.account.address, 0n], blockNumber: safe.number }) as bigint,
        userOperationEvidence: located ? await recoverCanonicalInclusion({ client, chainId: 421614, entryPoint: stack.entryPoint.address,
          sender: stack.account.address, operationHash, context: { ...context, transactionHash: located }, safeBlock: safe }) : { kind: 'not-located' },
      }
    },
    smartAccount: {
      accountAddress: stack.account.address, entryPoint: stack.entryPoint.address,
      prepareUserOperation: vi.fn(async ({ calls }) => {
        expect(calls).toHaveLength(1)
        const operation: ManagedUserOperation = { sender: stack.account.address,
          nonce: await read(stack.entryPoint, 'getNonce', [stack.account.address, 0n]) as bigint,
          callData: encodeFunctionData({ abi: stack.account.abi, functionName: 'execute', args: [calls[0].to, calls[0].value, calls[0].data] }),
          callGasLimit: 3_000_000n, verificationGasLimit: 300_000n, preVerificationGas: 60_000n,
          maxFeePerGas: 1_000_000_000n, maxPriorityFeePerGas: 1_000_000_000n,
          paymaster: stack.paymaster.address, paymasterVerificationGasLimit: 100_000n, paymasterPostOpGasLimit: 0n, signature: '0x',
        }
        const timestamp = await now()
        // Stub and final issuance both exercise the production Haskell policy.
        const stubDeadline = policyDeadline(timestamp, timestamp + 600n, operation.callData)
        const finalDeadline = policyDeadline(timestamp, timestamp + 600n, operation.callData)
        expect(stubDeadline).toBe(request.bounds.submitBy); expect(finalDeadline).toBe(stubDeadline)
        const prefix = concatHex([numberToHex(finalDeadline, { size: 6 }), numberToHex(timestamp - 30n, { size: 6 }),
          numberToHex(10n ** 18n, { size: 16 }), stack.policyId as Hex, stack.accountHash])
        operation.paymasterData = concatHex([prefix, `0x${'00'.repeat(65)}`])
        const digest = await read(stack.paymaster, 'getSponsorshipHash', [toPackedUserOperation(operation)]) as Hex
        operation.paymasterData = concatHex([prefix, await sponsor.sign({ hash: digest })])
        return operation
      }),
      signUserOperation: vi.fn(async operation => {
        await warp(await now() + (options.delay ?? 75n))
        await stack.mark() // The independent oracle worker continues during wallet approval.
        return { ...operation, signature: await owner.sign({ hash: hash(operation) }) }
      }),
      getUserOperationHash: hash,
      sendUserOperation: vi.fn(async operation => {
        policyDeadline(await now(), request.bounds.submitBy, operation.callData)
        if (options.commitAt) await rpc('evm_setNextBlockTimestamp', [Number(options.commitAt)])
        const tx = await write(stack.entryPoint, 'handleOps', [[toPackedUserOperation(operation)], keeper.address], keeper)
        const [log] = parseEventLogs({ abi: [event], logs: tx.logs })
        expect(log.args.userOpHash).toBe(hash(operation))
        expect(log.args.success).toBe(true)
        receipt = { ...log.args, entryPoint: stack.entryPoint.address, logs: tx.logs, receipt: tx } as ManagedUserOperationReceipt
        if (options.lostResponse) throw new Error('Local fault injection: response lost after inclusion')
        return hash(operation)
      }),
      getUserOperationStatus: vi.fn(async () => ({ status: 'not_found' as const, transactionHash: null })),
      getUserOperationReceipt: vi.fn(async () => receipt),
    },
  }
  return managed
}
const actionFor = (request: PerpsOrderRequestV3) => buildPlaceOrderV3Action({ account: stack.account.address, orderRouter: stack.router.address, request })
const run = (request: PerpsOrderRequestV3, managed: PerpsAaSmartAccountRuntime, action = actionFor(request)) => executeSponsoredPerpsAction({
  manifest, ownerAddress: owner.address, action, runtime: managed, orderRequestV3: persistPerpsOrderRequestV3(stack.account.address, request),
})
const timing = (id = 1n) => read(stack.book, 'orderTiming', [id]) as Promise<{ submitBy: bigint; executionWindowSeconds: number; commitTimestamp: bigint; executionDeadline: bigint }>
const outcome = (id = 1n) => read(stack.book, 'outcome', [id]) as Promise<{ status: number; reason: number; timing: unknown }>

describe('local V3 signed approval → sponsorship → commitment → execution → recovery', () => {
  it('recovers a lost submission response after slow approval and executes after submitBy without another signature', async () => {
    const request = await review()
    expect(request.bounds.executionWindowSeconds).toBe(60)
    expect(request.bounds.submitBy - await now()).toBe(120n)
    const managed = runtime(request, { lostResponse: true, commitAt: request.bounds.submitBy - 1n })
    await expect(run(request, managed)).rejects.toThrow('response lost')
    const committed = await timing()
    expect(committed.commitTimestamp).toBe(request.bounds.submitBy - 1n)
    expect(committed.executionDeadline - committed.commitTimestamp).toBe(60n)
    expect(await read(stack.router, 'nextCommitId')).toBe(2n)
    expect(localStorage.length).toBeGreaterThan(0)
    const disk = Object.fromEntries(Array.from({ length: localStorage.length }, (_, i) => {
      const key = localStorage.key(i)!; return [key, localStorage.getItem(key)!]
    }))
    useSponsoredOperationStore.setState({ operations: [] })
    for (const [key, value] of Object.entries(disk)) localStorage.setItem(key, value)
    await useSponsoredOperationStore.persist.rehydrate()
    restoreSponsoredOperationLane({ chainId: 421614, accountAddress: stack.account.address, lane: 'default' })
    const saved = useSponsoredOperationStore.getState().operations[0]
    expect(saved.status).toBe('submission-unknown')
    expect(saved.orderRequestV3).toEqual(persistPerpsOrderRequestV3(stack.account.address, request))
    await warp(request.bounds.submitBy + 10n)
    await rpc('anvil_mine', ['0x40', '0x0'])
    expect(await resolveProtocolOperation({ operation: saved, runtime: managed, userOperationHash: saved.userOperationHash! })).toMatchObject({ status: 'confirmed' })
    await warp(committed.executionDeadline - 10n)
    const executed = await stack.execute(1n, committed.commitTimestamp + 1n, committed.executionDeadline)
    expect((await client.getBlock({ blockNumber: executed.blockNumber })).timestamp).toBe(committed.executionDeadline)
    expect(await outcome()).toMatchObject({ status: 2, reason: 1, timing: committed })
    expect(await timing()).toEqual(committed)
    expect(managed.smartAccount.signUserOperation).toHaveBeenCalledTimes(1)
    expect(managed.smartAccount.sendUserOperation).toHaveBeenCalledTimes(1)
    await warp(committed.executionDeadline + 10n)
    expect((await client.simulateContract({ ...stack.router, functionName: 'commitOrder', args: [request], account: stack.account.address })).result).toBe(1n)
    await expect(client.simulateContract({ ...stack.router, functionName: 'commitOrder', args: [{ ...request, sizeDelta: request.sizeDelta + 1n }], account: stack.account.address })).rejects.toThrow()
    expect(await timing()).toEqual(committed)
  })
  it('rejects changes to either signed clock or size at real EntryPoint validation', async () => {
    const request = await review()
    const managed = runtime(request)
    const prepared = await managed.smartAccount.prepareUserOperation({ calls: actionFor(request).calls, action: 'place-order' })
    const signed = await managed.smartAccount.signUserOperation(prepared)
    for (const changed of [
      { ...request, bounds: { ...request.bounds, submitBy: request.bounds.submitBy + 1n } },
      { ...request, bounds: { ...request.bounds, executionWindowSeconds: 61 } },
      { ...request, sizeDelta: request.sizeDelta + 1n },
    ]) {
      const call = actionFor(changed).calls[0]
      const mutated = { ...signed, callData: encodeFunctionData({ abi: stack.account.abi, functionName: 'execute', args: [call.to, call.value, call.data] }) }
      await expect(client.simulateContract({ ...stack.entryPoint, functionName: 'handleOps', args: [[toPackedUserOperation(mutated)], keeper.address], account: keeper.address }))
        .rejects.toThrow(/AA24|AA34/)
    }
    expect(await read(stack.router, 'nextCommitId')).toBe(1n)
  })
  it('keeps a too-late approval journaled until canonical unused-authority expiry', async () => {
    const request = await review()
    const managed = runtime(request, { delay: 95n })
    await expect(run(request, managed)).rejects.toThrow()
    expect(managed.smartAccount.signUserOperation).toHaveBeenCalledTimes(1)
    expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
    expect(await read(stack.router, 'nextCommitId')).toBe(1n)
    await useSponsoredOperationStore.persist.rehydrate()
    const saved = useSponsoredOperationStore.getState().operations[0]
    expect(saved.userOperationHash).toBeDefined()
    expect(await resolveProtocolOperation({ operation: saved, runtime: managed, userOperationHash: saved.userOperationHash! })).toBeUndefined()
    await warp(request.bounds.submitBy + 1n)
    await rpc('anvil_mine', ['0x40', '0x0'])
    expect(await resolveProtocolOperation({ operation: saved, runtime: managed, userOperationHash: saved.userOperationHash! })).toEqual({ status: 'expired' })
    expect(managed.smartAccount.signUserOperation).toHaveBeenCalledTimes(1)
  })
  it('expires a committed order at its stored deadline and preserves terminal timing', async () => {
    const request = await review()
    const managed = runtime(request)
    await run(request, managed)
    const committed = await timing()
    await warp(committed.executionDeadline + 1n)
    await stack.execute(1n, committed.commitTimestamp + 1n)
    expect(await outcome()).toMatchObject({ status: 3, reason: 2, timing: committed })
    expect(await read(stack.router, 'accountHeadOrderId', [stack.account.address])).toBe(0n)
  })
  it('requires historical prices after commitment, independently of review time', async () => {
    const request = await review()
    const managed = runtime(request)
    await run(request, managed)
    const committed = await timing()
    await warp(committed.commitTimestamp + 5n)
    await expect(stack.execute(1n, committed.commitTimestamp - 1n)).rejects.toThrow()
    expect(await outcome()).toMatchObject({ status: 0 })
    expect(await read(stack.router, 'accountHeadOrderId', [stack.account.address])).toBe(1n)
    await stack.execute(1n, committed.commitTimestamp + 1n)
    expect(await outcome()).toMatchObject({ status: 2, reason: 1 })
  })
  it('uses the same clocks for protected opens through the shared SDK and backend parser', async () => {
    const request = await review()
    const action = buildProtectedOpenAction({ account: stack.account.address, book: stack.protection.address, request,
      params: { takeProfitTriggerPrice: 90_000_000n, stopLossTriggerPrice: 110_000_000n } })
    const managed = runtime(request)
    await run(request, managed, action)
    const committed = await timing()
    expect(committed.executionDeadline - committed.commitTimestamp).toBe(60n)
    await warp(request.bounds.submitBy + 1n)
    await stack.execute(1n, committed.commitTimestamp + 1n)
    expect(await outcome()).toMatchObject({ status: 2, reason: 1 })
    expect(await read(stack.protection, 'activePositionProtectionId', [stack.account.address])).toBeGreaterThan(0n)
    expect(managed.smartAccount.signUserOperation).toHaveBeenCalledTimes(1)
  })
})
