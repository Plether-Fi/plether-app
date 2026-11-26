import assert from 'node:assert/strict'
import test from 'node:test'
import { createRequire } from 'node:module'
import {
  assertCleanNonce, assertDepositEvents, assertSpend, checkedAmount, dedicated,
  depositTxHash, inbox, bridge, l1ToL2Alias, maximumAmountWei, readBalances, reserveWei,
} from './bridge-sepolia.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { concatHex, encodeAbiParameters, encodeEventTopics, keccak256, parseAbi,
  parseAbiParameters, parseEther, toHex } = require('viem')

const inboxEvent = parseAbi(['event InboxMessageDelivered(uint256 indexed messageNum, bytes data)'])
const bridgeEvent = parseAbi([
  'event MessageDelivered(uint256 indexed messageIndex, bytes32 indexed beforeInboxAcc, address inbox, uint8 kind, address sender, bytes32 messageDataHash, uint256 baseFeeL1, uint64 timestamp)',
])

function depositReceipt({ to = dedicated, amount = parseEther('2.48'), kind = 12,
  bridgeSender = l1ToL2Alias(dedicated), messageIndex = 431n } = {}) {
  const payload = concatHex([to, toHex(amount, { size: 32 })])
  return { logs: [
    { address: inbox, topics: encodeEventTopics({ abi: inboxEvent,
      eventName: 'InboxMessageDelivered', args: { messageNum: messageIndex } }),
    data: encodeAbiParameters(parseAbiParameters('bytes'), [payload]) },
    { address: bridge, topics: encodeEventTopics({ abi: bridgeEvent,
      eventName: 'MessageDelivered', args: {
        messageIndex, beforeInboxAcc: `0x${'01'.repeat(32)}`,
      } }),
    data: encodeAbiParameters(parseAbiParameters('address,uint8,address,bytes32,uint256,uint64'),
      [inbox, kind, bridgeSender, keccak256(payload), 1n, 2n]) },
  ] }
}

test('caps bridge amount and preserves a post-gas L1 reserve', () => {
  assert.equal(checkedAmount(String(parseEther('2.48'))), parseEther('2.48'))
  assert.equal(checkedAmount(String(maximumAmountWei)), maximumAmountWei)
  assert.throws(() => checkedAmount(String(maximumAmountWei + 1n)), /ceiling/u)
  assert.throws(() => checkedAmount('2.48'), /decimal Wei/u)
  const spend = assertSpend({ balanceWei: parseEther('2.5'), amountWei: parseEther('2.48'),
    gas: 250_000n, maxFeePerGas: 10_000_000_000n })
  assert.equal(spend.maxGasCostWei, parseEther('0.0025'))
  assert.equal(spend.minimumRemainingWei, parseEther('0.0175'))
  assert.ok(spend.minimumRemainingWei >= reserveWei)
  assert.throws(() => assertSpend({ balanceWei: parseEther('2.5'),
    amountWei: parseEther('2.49'), gas: 250_000n, maxFeePerGas: 10_000_000_000n }),
  /less than 0.01 ETH/u)
  assert.throws(() => assertSpend({ balanceWei: parseEther('2.5'),
    amountWei: parseEther('2.48'), gas: 250_001n, maxFeePerGas: 10_000_000_000n }),
  /gas limit/u)
})

test('rejects any pending or already-used source nonce', () => {
  assert.equal(assertCleanNonce([0, 0, 0, 0]), 0n)
  assert.throws(() => assertCleanNonce([0, 1, 0, 1]), /pending/u)
  assert.throws(() => assertCleanNonce([0, 0, 1, 1]), /disagree/u)
  assert.throws(() => assertCleanNonce([1, 1, 1, 1]), /nonce differs/u)
})

test('compares the same safe and latest balance across independent RPCs', async () => {
  const safe = { number: 200n }
  function client(safeWei, latestWei) {
    return { async getBalance({ blockNumber }) { return blockNumber === safe.number ? safeWei : latestWei } }
  }
  assert.deepEqual(await readBalances([client(10n, 11n), client(10n, 11n)], dedicated, safe, 'test'),
    { safeWei: 10n, latestWei: 11n })
  await assert.rejects(readBalances([client(10n, 11n), client(9n, 11n)], dedicated, safe, 'test'),
    /disagree on safe balance/u)
  await assert.rejects(readBalances([client(10n, 11n), client(10n, 12n)], dedicated, safe, 'test'),
    /disagree on latest balance/u)
})

test('links the one L1 Inbox/Bridge message to a deterministic type-0x64 child hash', () => {
  const amount = parseEther('2.48')
  const decoded = assertDepositEvents(depositReceipt(), amount)
  assert.equal(decoded.messageIndex, 431n)
  assert.equal(decoded.value, amount)
  assert.equal(decoded.recipient, dedicated)
  assert.equal(decoded.fromAlias, l1ToL2Alias(dedicated))
  // Independently derived from Offchain Labs' SDK RLP field order and 0x64 envelope.
  assert.equal(decoded.childTxHash,
    '0x59daf158ade33a29ce931a11c67246eec7beccf9c8090390481b78013c5a0d5e')
  assert.equal(decoded.childTxHash, depositTxHash(431n, decoded.fromAlias, dedicated, amount))
  assert.match(decoded.childTxHash, /^0x[0-9a-f]{64}$/u)
})

test('derives the observed Arbitrum Sepolia child hash for a live L1 ETH deposit', () => {
  // L1 tx: 0x2d380900b4e112bfc0531bbb50d8754a8df1e3d64c335e2064d6fb2b7eba7d72
  // Its MessageDelivered index, alias, recipient, and value produce this observed L2 tx.
  assert.equal(depositTxHash(1_370_474n,
    '0x9Ca0b32934cc9b0Fe5b18e53C9e4456Bff0Bd6DC',
    '0x8B8FB32934cC9b0fe5B18e53c9E4456Bff0BC5cB',
    parseEther('0.1')),
  '0x6783d4b167b815bc7e61acec786af107c5802526efe5500aef0fa9a90a8b67db')
})

test('rejects an aliased destination or mismatched bridge kind', () => {
  const amount = parseEther('2.48')
  assert.throws(() => assertDepositEvents(depositReceipt({ to: l1ToL2Alias(dedicated) }), amount),
    /destination changed/u)
  assert.throws(() => assertDepositEvents(depositReceipt({ kind: 9 }), amount),
    /not an ETH deposit/u)
  assert.throws(() => assertDepositEvents(depositReceipt({ bridgeSender: dedicated }), amount),
    /sender is not/u)
})

test('rejects a Bridge event whose message hash is not the Inbox payload', () => {
  const receipt = depositReceipt()
  const wrong = encodeAbiParameters(parseAbiParameters('address,uint8,address,bytes32,uint256,uint64'),
    [inbox, 12, l1ToL2Alias(dedicated), `0x${'ff'.repeat(32)}`, 1n, 2n])
  receipt.logs[1].data = wrong
  assert.throws(() => assertDepositEvents(receipt, parseEther('2.48')), /message hash differs/u)
})
