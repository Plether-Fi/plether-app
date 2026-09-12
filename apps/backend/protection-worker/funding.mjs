// Read-only reserve monitoring. Never used as a transaction authorization.
import { parseTransaction, recoverTransactionAddress } from 'viem'

export const components = ['alto', 'keeper', 'oracle', 'liquidation', 'protection', 'lp_settlement']
const uint = value => {
  if (typeof value !== 'string' || !/^(0|[1-9][0-9]*)$/.test(value) || value.length > 78) throw new Error('Invalid funding configuration')
  return BigInt(value)
}
export function parseMonitors(raw) {
  const values = JSON.parse(raw)
  if (!Array.isArray(values) || !values.length || values.length > 16) throw new Error('Invalid funding inventory')
  const seen = new Set()
  return values.map(v => {
    if (!v || Object.keys(v).sort().join(',') !== 'address,component,feeBufferBps,gasLimit,valueWei'
      || !components.includes(v.component) || !/^0x[0-9a-fA-F]{40}$/.test(v.address) || /^0x0{40}$/.test(v.address)) throw new Error('Invalid funding inventory')
    const address = v.address.toLowerCase()
    // Shared signer inventories would incorrectly allocate the same reserve twice.
    if (seen.has(address)) throw new Error('Funding signers must be dedicated')
    seen.add(address)
    const gasLimit = uint(v.gasLimit), valueWei = uint(v.valueWei), feeBufferBps = uint(v.feeBufferBps)
    if (gasLimit < 21000n || gasLimit > 1000000000n || feeBufferBps > 100000n) throw new Error('Invalid funding bounds')
    return { component: v.component, address, gasLimit, valueWei, feeBufferBps }
  })
}

// A replacement family consumes one nonce, so reserve its maximum, never its
// sum. Included nonces are already reflected in the fixed-block balance.
export function pendingLiability(confirmed, pending, transactions) {
  if (confirmed < 0n || pending < confirmed || pending-confirmed > 1024n || transactions.length > 4096) return undefined
  const families = new Map()
  for (const tx of transactions) {
    if (tx.nonce < 0n || tx.cost < 0n) return undefined
    if (tx.nonce < confirmed) continue
    families.set(tx.nonce, tx.cost > (families.get(tx.nonce) ?? 0n) ? tx.cost : (families.get(tx.nonce) ?? 0n))
  }
  for (let n = confirmed; n < pending; n++) if (!families.has(n)) return undefined
  // Include signed but not yet broadcast and queued transactions above pending.
  return [...families.values()].reduce((a,b) => a+b, 0n)
}

export function classifyReserve({ balance, liability, reserve, timestamp, now }) {
  if (balance < 0n || liability === undefined || liability < 0n || reserve <= 0n || timestamp > now+2n || now-timestamp > 15n) return { state: 'unknown', reason: 'FUNDING_UNVERIFIED' }
  if (balance === 0n) return { state: 'blocked', reason: 'WORKER_INSUFFICIENT_FUNDS' }
  // A conservative cap is not a lower bound on an actual execution's cost.
  if (balance-liability < reserve) return { state: 'unknown', reason: 'FUNDING_LOW' }
  return { state: 'ready', reason: balance-liability < reserve*10n ? 'FUNDING_LOW' : 'READY' }
}

export function transactionLiability(tx) {
  const nonce = BigInt(tx.nonce), gas = BigInt(tx.gas), value = BigInt(tx.value)
  const fee = BigInt(tx.maxFeePerGas ?? tx.gasPrice)
  if (nonce < 0n || gas <= 0n || value < 0n || fee <= 0n) throw new Error('Invalid transaction evidence')
  return { nonce, cost: value + gas*fee }
}

// Read each existing journal; no new ownership of recovery or signed bytes.
export async function journalLiabilities(db, chainId, release, monitor) {
  const address = monitor.address
  let rows
  if (monitor.component === 'lp_settlement') {
    rows = (await db.query(`SELECT tx_nonce::text AS nonce, (tx_value+gas_limit*max_fee_per_gas)::text AS cost
      FROM perps_lp_settlement_transactions WHERE chain_id=$1 AND monitor_address=$2 AND signer_address=$3
      AND status NOT IN ('confirmed_success','confirmed_revert','superseded') LIMIT 4097`,
    [chainId, release.contracts.settlementMonitorLens.address.toLowerCase(), address])).rows
  } else if (monitor.component === 'liquidation') {
    rows = (await db.query(`SELECT pending_nonce::text AS nonce, (pending_value+pending_gas_limit*pending_max_fee_per_gas)::text AS cost
      FROM perps_liquidation_candidates WHERE chain_id=$1 AND cfd_engine=$2 AND pending_sender=$3 AND pending_tx_hash IS NOT NULL LIMIT 4097`,
    [chainId, release.contracts.cfdEngine.address.toLowerCase(), address])).rows
  } else if (monitor.component === 'protection') {
    const rawRows = (await db.query(`SELECT raw_transaction FROM perps_protection_transactions
      WHERE chain_id=$1 AND book=$2 AND status IN ('pending','included') LIMIT 4097`,
    [chainId, release.contracts.positionProtectionBook.address.toLowerCase()])).rows
    if (rawRows.length > 4096) throw new Error('Journal bound exceeded')
    return Promise.all(rawRows.map(async row => {
      const tx = parseTransaction(row.raw_transaction)
      const sender = await recoverTransactionAddress({ serializedTransaction: row.raw_transaction })
      if (sender.toLowerCase() !== address || tx.chainId !== chainId) throw new Error('Journal signer mismatch')
      return transactionLiability(tx)
    }))
  } else return []
  if (rows.length > 4096) throw new Error('Journal bound exceeded')
  return rows.map(row => ({ nonce: uint(row.nonce), cost: uint(row.cost) }))
}

export async function observeFunding({ client, db, release, monitors, previous, now = () => BigInt(Math.floor(Date.now()/1000)) }) {
  const chainId = release.network.chainId
  if (await client.getChainId() !== chainId) throw new Error('Funding chain mismatch')
  const block = await client.getBlock({ blockTag: 'latest' })
  if (block.number === null || !block.hash || previous && (block.number < previous.number || block.number === previous.number && block.hash !== previous.hash)) throw new Error('Funding head changed')
  const [gasPrice, priorityFee, pendingBlock] = await Promise.all([
    client.getGasPrice(), client.estimateMaxPriorityFeePerGas().catch(() => undefined),
    client.getBlock({ blockTag: 'pending', includeTransactions: true }).catch(() => undefined),
  ])
  const fee = priorityFee === undefined || gasPrice > priorityFee ? gasPrice : priorityFee
  if (gasPrice <= 0n || fee <= 0n || priorityFee !== undefined && priorityFee < 0n) throw new Error('Invalid fee evidence')
  const results = await Promise.all(monitors.map(async monitor => {
    try {
      const [balance, confirmed, pending, journal] = await Promise.all([
        client.getBalance({ address: monitor.address, blockNumber: block.number }),
        client.getTransactionCount({ address: monitor.address, blockNumber: block.number }),
        client.getTransactionCount({ address: monitor.address, blockTag: 'pending' }),
        journalLiabilities(db, chainId, release, monitor),
      ])
      let txs = []
      if (pendingBlock?.parentHash === block.hash && pendingBlock.number === block.number+1n) {
        if (pendingBlock.transactions.length > 4096) throw new Error('Pending block bound exceeded')
        // Hash-only responses cannot certify pending liabilities.
        if (pendingBlock.transactions.some(tx => typeof tx !== 'object')) throw new Error('Pending bodies unavailable')
        txs = pendingBlock.transactions.filter(tx => tx.from.toLowerCase() === monitor.address).map(transactionLiability)
      }
      const liability = pendingLiability(BigInt(confirmed), BigInt(pending), [...journal,...txs])
      const maxFee = (fee*(10000n+monitor.feeBufferBps)+9999n)/10000n
      const reserve = monitor.gasLimit*maxFee+monitor.valueWei
      return { ...monitor, ...classifyReserve({ balance, liability, reserve, timestamp: block.timestamp, now: now() }), balance, liability, reserve }
    } catch { return { ...monitor, state: 'unknown', reason: 'FUNDING_UNVERIFIED' } }
  }))
  const canonical = await client.getBlock({ blockNumber: block.number })
  if (canonical.hash !== block.hash || now()-block.timestamp > 15n || block.timestamp > now()+2n) throw new Error('Funding snapshot not canonical or fresh')
  return { block, results }
}
