import { AttributionError } from './accounting.mjs'
import fs from 'node:fs'
import { decodeFunctionData, encodeFunctionData, keccak256, toHex } from 'viem'
const artifact = JSON.parse(fs.readFileSync(new URL('./settlement-abi.json', import.meta.url), 'utf8'))
export const settlementAbi = artifact.abi
export const SOURCE_COMMIT = artifact.sourceCommit
export const topic = signature => keccak256(toHex(signature))
export const TOPICS = {
  open: topic('PositionOpened(address,uint8,uint256,uint256,uint256)'),
  close: topic('PositionClosed(address,uint8,uint256,uint256,int256)'),
  liquidation: topic('PositionLiquidated(address,uint8,uint256,uint256,uint256)'),
  carry: topic('CarryRealized(address,uint256,uint256,uint256,uint256)'),
  frozen: topic('FrozenCloseSpreadSettled(address,uint256,uint256,uint256)'),
  deposit: topic('DepositEpochFinalized(uint256,bool,uint256,uint256)'),
  redeem: topic('RedeemEpochFunded(uint256,bool,uint256,uint256)'),
  seed: topic('SeedPositionInitialized(bool,address,uint256,uint256)'),
  assigned: topic('UnassignedAssetsAssigned(bool,address,uint256,uint256)'),
}
export function words(data, count) {
  if (typeof data !== 'string' || !/^0x[\da-f]*$/i.test(data) || (data.length - 2) % 64 || (count !== undefined && data.length !== 2 + count * 64)) throw new AttributionError('Malformed ABI evidence')
  return Array.from({ length: (data.length - 2) / 64 }, (_unused, index) => BigInt('0x' + data.slice(2 + index * 64, 66 + index * 64)))
}
export const addressTopic = value => ('0x' + value.slice(-40)).toLowerCase()
const same = (a, b) => typeof a === 'string' && typeof b === 'string' && a.toLowerCase() === b.toLowerCase()
const selector = signature => topic(signature).slice(0, 10)
const ORACLE_CALLS = new Map([
  ['updateOrderExecutionPrice(address,bytes[],(uint64,uint256,uint8,bool,bool))', [9, 1]],
  ['updateBatchOrderExecutionPrice(address,bytes[],(uint64,uint256,uint8,bool,bool),(bool,uint64,uint256,uint256,uint64,uint64))', [15, 1]],
  ['updatePrice(address,bytes[],uint8)', [8, 0]],
  ['updateLiquidationPrice(address,bytes[],address)', [8, 0]],
  ['updateLiquidationBatchPrice(address,bytes[])', [5, 0]],
].map(([signature, shape]) => [selector(signature), shape]))
const kindForTopic = value => value === TOPICS.open ? 'Open' : value === TOPICS.close ? 'Close' : value === TOPICS.liquidation ? 'Liquidation' : undefined

// Only successful Engine -> bound Sidecar CALLs may supply economics. Receipt
// identities are verified against canonical blocks by the RPC/cache layer.
export function decodeActions(trace, receiptLogs, release) {
  const engine = release.contracts.cfdEngine.address
  const sidecar = release.contracts.cfdEngineSettlementSidecar.address
  const router = release.contracts.orderRouter.address
  const oracle = release.contracts.pletherOracle.address
  const quotes = []
  const actions = []
  function visit(node, succeeded = true) {
    if (!node || typeof node !== 'object') throw new AttributionError('Malformed call trace')
    if (!succeeded || node.error || node.failed) return
    if (node.type === 'CALL' && same(node.from, router) && same(node.to, oracle) && node.output) {
      const shape = ORACLE_CALLS.get(node.input?.slice(0, 10))
      if (shape) {
        const [length, offset] = shape
        const output = words(node.output, length)
        if (!offset || output[0] === 1n) {
          const q = output.slice(offset)
          quotes.push(length === 5 ? { prices: [q[0], q[1]], mark: q[2], time: q[3] }
            : { prices: [q[0]], mark: q[1], time: q[2] })
        }
      }
    }
    if (node.type === 'CALL' && same(node.from, engine) && same(node.to, sidecar)) {
      let decoded
      try { decoded = decodeFunctionData({ abi: settlementAbi, data: node.input }) } catch { decoded = undefined }
      if (decoded) {
        const kind = decoded.functionName.slice('execute'.length)
        const [delta, positionOrTime, time] = decoded.args
        const publishTime = kind === 'Liquidation' ? positionOrTime : time
        const quote = [...quotes].reverse().find(item => item.time === BigInt(publishTime) && item.prices.includes(BigInt(delta.price)))
        if (!quote || quote.mark <= 0n) throw new AttributionError('Trade has no bound neutral oracle quote')
        if ((kind === 'Liquidation' && !delta.liquidatable) || (kind !== 'Liquidation' && (!delta.valid || Number(delta.revertCode) !== 0))) throw new AttributionError('Unsuccessful settlement delta')
        actions.push({ kind, delta, markPrice: quote.mark, frozenSpreadPaid: 0n })
      }
    }
    for (const child of node.calls ?? []) visit(child, true)
  }
  visit(trace)
  const positions = receiptLogs.filter(log => same(log.address, engine) && kindForTopic(log.topics[0]))
  if (positions.length !== actions.length) throw new AttributionError('Settlement trace does not match position receipts')
  actions.forEach((action, index) => {
    const position = positions[index]
    if (kindForTopic(position.topics[0]) !== action.kind || addressTopic(position.topics[1]) !== action.delta.account.toLowerCase()) throw new AttributionError('Settlement account/order mismatch')
    const data = words(position.data)
    const size = BigInt(action.kind === 'Liquidation' ? action.delta.posSize : action.delta.sizeDelta)
    if (data[0] !== BigInt(action.kind === 'Open' ? action.delta.posSide : action.delta.side) || data[1] !== size || data[2] !== BigInt(action.delta.price)) throw new AttributionError('Settlement size/price mismatch')
    action.logIndex = Number(BigInt(position.logIndex))
    const previous = index > 0 ? Number(BigInt(positions[index - 1].logIndex)) : -1
    const spreads = receiptLogs.filter(log => same(log.address, sidecar) && log.topics[0] === TOPICS.frozen
      && Number(BigInt(log.logIndex)) > previous && Number(BigInt(log.logIndex)) < action.logIndex
      && addressTopic(log.topics[1]) === action.delta.account.toLowerCase())
    if (spreads.length > 1) throw new AttributionError('Duplicate frozen spread evidence')
    action.frozenSpreadPaid = spreads.length ? words(spreads[0].data, 3)[1] : 0n
  })
  return actions
}

export function capitalEvent(log, release) {
  const index = Number(BigInt(log.logIndex))
  if (same(log.address, release.contracts.housePool.address)
    && [TOPICS.seed, TOPICS.assigned].includes(log.topics[0]) && BigInt(log.topics[1]) === 0n) {
    const [assets, shares] = words(log.data, 2)
    return { shareDelta: shares, assetDelta: assets, logIndex: index }
  }
  if (!same(log.address, release.contracts.juniorVault.address)) return undefined
  if (log.topics[0] === TOPICS.deposit) {
    const [assets, shares] = words(log.data, 2)
    return { shareDelta: shares, assetDelta: assets, logIndex: index }
  }
  if (log.topics[0] === TOPICS.redeem) {
    const [shares, assets] = words(log.data, 2)
    return { shareDelta: -shares, assetDelta: -assets, logIndex: index }
  }
  return undefined
}

// The pinned pool reads terminal NAV before funding redemptions or minting
// deposits. Read that value from the successful call trace, rather than assuming
// a mark movement happened before/after every capital flow in the same block.
export function decodeLpValuations(trace, receiptLogs, release) {
  const pool = release.contracts.housePool.address
  const engine = release.contracts.cfdEngine.address
  const junior = release.contracts.juniorVault.address
  const settle = selector('settleLpEpoch(uint256,uint256)')
  const nav = selector('terminalNavSnapshot()')
  const checkpoint = selector('checkpointCarryIndexes()')
  const deposit = selector('finalizeDepositEpochFromPool(uint256,uint256)')
  const redeem = selector('fundRedeemEpoch(uint256,uint256,uint256)')
  const flows = []
  function visit(node) {
    if (!node || typeof node !== 'object') throw new AttributionError('Malformed LP call trace')
    if (node.error || node.failed) return
    if (node.type === 'CALL' && same(node.to, pool) && node.input?.slice(0, 10) === settle) {
      words('0x' + node.input.slice(10), 2)
      let terminal
      function settlement(child) {
        if (child.error || child.failed) return
        const id = child.input?.slice(0, 10)
        if (same(child.to, engine)) {
          if (child.type === 'STATICCALL' && id === nav) {
            const value = words(child.output, 8)
            // Frozen/no-position callers may pass unused mark arguments. The
            // independent Engine return is authoritative for both live/frozen LP settlement.
            if (value[7] !== 0n || value[6] > 1n || (value[6] === 0n && value[2] !== 0n)) throw new AttributionError('Invalid LP terminal valuation')
            const pnl = value[2] >= 2n ** 255n ? value[2] - 2n ** 256n : value[2]
            if (terminal !== undefined && terminal !== pnl) throw new AttributionError('Terminal NAV changed inside LP settlement')
            terminal = pnl
          } else if (child.type === 'CALL' && id !== checkpoint) throw new AttributionError('Unexpected engine mutation inside LP settlement')
        }
        if (child.type === 'CALL' && same(child.from, pool) && same(child.to, junior) && [deposit, redeem].includes(id)) {
          if (terminal === undefined) throw new AttributionError('Capital flow has no preceding terminal NAV evidence')
          const args = words('0x' + child.input.slice(10), id === deposit ? 2 : 3)
          if (args[1] > 0n) flows.push({ kind: id === deposit ? TOPICS.deposit : TOPICS.redeem,
            epoch: args[0], shares: args[1], assets: id === deposit ? words(child.output, 1)[0] : args[2], terminalPnlBeforeFlow: terminal })
        }
        for (const nested of child.calls ?? []) settlement(nested)
      }
      for (const child of node.calls ?? []) settlement(child)
      return
    }
    for (const child of node.calls ?? []) visit(child)
  }
  visit(trace)
  const receipts = receiptLogs.filter(log => same(log.address, junior) && [TOPICS.deposit, TOPICS.redeem].includes(log.topics[0]))
  if (flows.length !== receipts.length) throw new AttributionError('LP valuation trace does not match capital receipts')
  return flows.map((flow, index) => {
    const receipt = receipts[index]
    const actual = capitalEvent(receipt, release)
    const sign = flow.kind === TOPICS.deposit ? 1n : -1n
    if (receipt.topics[0] !== flow.kind || BigInt(receipt.topics[1]) !== flow.epoch || BigInt(receipt.topics[2]) !== 0n
      || actual.shareDelta !== sign * flow.shares || actual.assetDelta !== sign * flow.assets) throw new AttributionError('LP valuation capital identity mismatch')
    return { logIndex: actual.logIndex, terminalPnlBeforeFlow: flow.terminalPnlBeforeFlow }
  })
}
export function getter(name, args = []) {
  return encodeFunctionData({ abi: [{ type: 'function', name, stateMutability: 'view', inputs: args.length ? [{ name: 'shares', type: 'uint256' }] : [], outputs: [] }], functionName: name, args })
}
