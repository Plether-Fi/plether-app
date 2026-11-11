import { AttributionError } from './accounting.mjs'
import fs from 'node:fs'
import { keccak256, toHex } from 'viem'
import { buildAttribution, EntryLedger, json } from './accounting.mjs'
import { capitalEvent, decodeActions, decodeLpValuations, getter, SOURCE_COMMIT, TOPICS, words } from './evidence.mjs'
const scalar = value => words(value, 1)[0]
const signed = value => value >= 2n ** 255n ? value - 2n ** 256n : value
const same = (a, b) => a?.toLowerCase() === b?.toLowerCase()
const hex = value => '0x' + BigInt(value).toString(16)
const positionTopic = value => [TOPICS.open, TOPICS.close, TOPICS.liquidation].includes(value)
const reviveSnapshot = snapshot => Object.fromEntries(Object.entries(snapshot).map(([key, value]) => [key, typeof value === 'string' && /^\d+$/.test(value) || key === 'terminalPnl' ? BigInt(value) : value]))

export class ReturnAttributionWorker {
  constructor({ db, client, release, log = () => {} }) {
    this.db = db; this.client = client; this.release = release; this.log = log
    this.address = Object.fromEntries(Object.entries(release.contracts).map(([key, value]) => [key, value.address?.toLowerCase()]))
    this.key = keccak256(toHex(json({ source: SOURCE_COMMIT, chain: release.network.chainId,
      pool: this.address.housePool, engine: this.address.cfdEngine, router: this.address.orderRouter,
      junior: this.address.juniorVault, senior: this.address.seniorVault })))
    this.blocks = new Map()
  }
  async init() {
    if (this.release.release.sourceCommit !== SOURCE_COMMIT || this.release.release.phase !== 'active') throw new AttributionError('Unsupported return-attribution release')
    await this.db.query(fs.readFileSync(new URL('./schema.sql', import.meta.url), 'utf8'))
    const lock = await this.db.query('SELECT pg_try_advisory_lock($1,$2) AS acquired', [this.release.network.chainId, 1759])
    if (!lock.rows[0].acquired) return false
    if (await this.client.getChainId() !== this.release.network.chainId) throw new AttributionError('Wrong attribution chain')
    for (const name of ['housePool', 'cfdEngine', 'cfdEngineSettlementSidecar', 'juniorVault', 'seniorVault', 'orderRouter', 'pletherOracle']) {
      const code = await this.client.getCode({ address: this.address[name] })
      if (!code || keccak256(code) !== this.release.contracts[name].runtimeCodeHash) throw new AttributionError('Attribution runtime binding mismatch')
    }
    const decimals = await Promise.all(['settlementToken', 'seniorVault', 'juniorVault'].map(name => this.call(this.address[name], 'decimals', undefined)))
    if (decimals[0] !== 6n || decimals[1] !== 9n || decimals[2] !== 9n) throw new AttributionError('Unsupported asset/share decimals')
    return true
  }
  async block(number, fresh = false) {
    const key = number.toString()
    if (!fresh && this.blocks.has(key)) return this.blocks.get(key)
    const block = await this.client.getBlock({ blockNumber: number })
    if (block.number !== number || !block.hash) throw new AttributionError('Missing canonical block')
    this.blocks.set(key, block)
    return block
  }
  async call(address, name, blockNumber, args = []) {
    const result = await this.client.call({ to: address, data: getter(name, args), blockNumber })
    return scalar(result.data)
  }
  async cached(key, hash, produce) {
    const existing = await this.db.query('SELECT payload FROM vault_return_evidence WHERE deployment_key=$1 AND evidence_key=$2 AND block_hash=$3', [this.key, key, hash])
    if (existing.rows.length) return existing.rows[0].payload
    const value = await produce()
    await this.db.query('INSERT INTO vault_return_evidence(deployment_key,evidence_key,block_hash,payload) VALUES($1,$2,$3,$4::jsonb) ON CONFLICT DO NOTHING', [this.key, key, hash, json(value)])
    return JSON.parse(json(value))
  }
  async snapshot(number) {
    const block = await this.block(number)
    const data = await this.cached('snapshot:' + number, block.hash, async () => {
      const calls = [
        [this.address.juniorVault, 'totalAssets'], [this.address.juniorVault, 'accruedTotalSupply'],
        [this.address.juniorVault, 'totalSupply'], [this.address.juniorVault, 'convertToAssets', [10n ** 27n]],
        [this.address.housePool, 'seniorPrincipal'], [this.address.housePool, 'juniorPrincipal'],
        [this.address.housePool, 'seniorHighWaterMark'], [this.address.housePool, 'lastSeniorCouponCheckpointTime'],
        [this.address.housePool, 'seniorRateBps'], [this.address.cfdEngine, 'terminalNavSnapshot'],
        [this.address.housePool, 'getPoolLiquidityView'],
      ]
      // Each call is pinned to the same confirmed block. Pace batches rather
      // than depending on provider-specific JSON-RPC batch support.
      const output = []
      for (let offset = 0; offset < calls.length; offset += 4) {
        const batch = await Promise.all(calls.slice(offset, offset + 4).map(async ([to, name, args = []]) => {
          try {
            const response = await this.client.call({ to, data: getter(name, args), blockNumber: number })
            return response.data
          } catch (error) { this.log('error', 'vault_return_snapshot_read_failed', { block: number.toString(), getter: name, error: error.name }); throw error }
        }))
        output.push(...batch)
      }
      const terminal = words(output[9], 8)
      const liquidity = words(output[10], 12)
      const verified = await this.block(number, true)
      if (verified.hash !== block.hash) throw new AttributionError('Snapshot reorged during sampling')
      return { block: number, blockHash: block.hash, timestamp: block.timestamp,
        assets: scalar(output[0]), supply: scalar(output[1]), rawSupply: scalar(output[2]),
        sharePrice: scalar(output[3]) / 1000000n,
        rawSenior: scalar(output[4]), rawJunior: scalar(output[5]), highWaterMark: scalar(output[6]),
        couponCheckpoint: scalar(output[7]), seniorRateBps: scalar(output[8]), terminalPnl: signed(terminal[2]),
        markFresh: liquidity[9] === 1n }
    })
    return reviveSnapshot(data)
  }
  async index(end) {
    const state = await this.db.query('SELECT through_block,through_hash FROM vault_return_index_state WHERE deployment_key=$1', [this.key])
    let from = BigInt(this.release.release.deploymentBlock)
    if (state.rows.length) {
      const previous = state.rows[0]
      const block = await this.block(BigInt(previous.through_block), true)
      if (block.hash === previous.through_hash) from = block.number + 1n
      else {
        // Rebuild the entire release prefix; never retain a neutral entry book
        // or trace from an orphaned chain. Only derived index rows are deleted.
        await this.db.query('BEGIN')
        try {
          await this.db.query('DELETE FROM vault_return_logs WHERE deployment_key=$1', [this.key])
          await this.db.query('DELETE FROM vault_return_evidence WHERE deployment_key=$1', [this.key])
          await this.db.query('DELETE FROM vault_return_index_state WHERE deployment_key=$1', [this.key])
          await this.db.query('DELETE FROM vault_return_reports WHERE chain_id=$1 AND house_pool_address=$2', [this.release.network.chainId, this.address.housePool])
          await this.db.query('COMMIT')
        } catch (error) { await this.db.query('ROLLBACK'); throw error }
        this.blocks.clear()
      }
    }
    const addresses = ['cfdEngine', 'cfdEngineSettlementSidecar', 'orderRouter', 'housePool', 'juniorVault', 'seniorVault', 'terminalNavBookV2'].map(name => this.address[name])
    let batch = 20000n
    while (from <= end) {
      const to = from + batch - 1n < end ? from + batch - 1n : end
      let logs
      try { logs = await this.client.request({ method: 'eth_getLogs', params: [{ address: addresses, fromBlock: hex(from), toBlock: hex(to) }] }) }
      catch (error) { if (batch <= 100n) throw error; batch /= 2n; continue }
      if (!Array.isArray(logs)) throw new AttributionError('Invalid log response')
      const final = await this.block(to, true)
      const seen = new Set()
      for (const entry of logs) {
        const number = BigInt(entry.blockNumber)
        const identity = entry.blockNumber + ':' + entry.logIndex
        if (entry.removed || number < from || number > to || seen.has(identity) || !addresses.includes(entry.address.toLowerCase())) throw new AttributionError('Noncanonical/duplicate log')
        seen.add(identity)
        if ((await this.block(number)).hash !== entry.blockHash) throw new AttributionError('Log block hash mismatch')
      }
      if ((await this.block(to, true)).hash !== final.hash) throw new AttributionError('Log batch reorged')
      await this.db.query('BEGIN')
      try {
        for (const entry of logs) await this.db.query('INSERT INTO vault_return_logs(deployment_key,block_number,block_hash,transaction_hash,log_index,payload) VALUES($1,$2,$3,$4,$5,$6::jsonb) ON CONFLICT DO NOTHING', [this.key, BigInt(entry.blockNumber).toString(), entry.blockHash, entry.transactionHash, Number(BigInt(entry.logIndex)), json(entry)])
        await this.db.query('INSERT INTO vault_return_index_state VALUES($1,$2,$3) ON CONFLICT(deployment_key) DO UPDATE SET through_block=EXCLUDED.through_block,through_hash=EXCLUDED.through_hash', [this.key, to.toString(), final.hash])
        await this.db.query('COMMIT')
      } catch (error) { await this.db.query('ROLLBACK'); throw error }
      from = to + 1n
      this.log('info', 'vault_return_index_progress', { throughBlock: to.toString(), endBlock: end.toString() })
    }
  }
  async transactionEvidence(txHash, hash, number) {
    return this.cached('trace:' + txHash, hash, async () => {
      const receipt = await this.client.request({ method: 'eth_getTransactionReceipt', params: [txHash] })
      if (!receipt || receipt.transactionHash !== txHash || receipt.blockHash !== hash || BigInt(receipt.blockNumber) !== number || receipt.status !== '0x1') throw new AttributionError('Settlement receipt identity/status mismatch')
      const indices = receipt.logs.map(entry => entry.logIndex)
      if (new Set(indices).size !== indices.length || receipt.logs.some(entry => entry.transactionHash !== txHash || entry.blockHash !== hash || BigInt(entry.blockNumber) !== number)) throw new AttributionError('Receipt log identity mismatch')
      const trace = await this.client.request({ method: 'debug_traceTransaction', params: [txHash, { tracer: 'callTracer', timeout: '30s' }] })
      // Parse before caching, so failed/unsupported traces cannot poison a cache.
      decodeActions(trace, receipt.logs, this.release)
      if ((await this.block(number, true)).hash !== hash) throw new AttributionError('Settlement trace reorged')
      return { trace, logs: receipt.logs }
    })
  }
  async run() {
    this.blocks.clear()
    const eligibleEpoch = Math.floor((Date.now() / 1000 - 120) / 3600) * 3600
    const rows = (await this.db.query(`SELECT epoch_timestamp,block_number,block_hash,block_timestamp,junior_share_price_wad,mark_fresh FROM
      (SELECT * FROM vault_performance_snapshots WHERE chain_id=$1 AND house_pool_address=$2 AND senior_vault_address=$3 AND junior_vault_address=$4
       AND epoch_timestamp <= $5 ORDER BY epoch_timestamp DESC LIMIT 169) AS recent ORDER BY epoch_timestamp`,
    [this.release.network.chainId, this.address.housePool, this.address.seniorVault, this.address.juniorVault, eligibleEpoch])).rows
    if (rows.length !== 169 || Number(rows.at(-1).epoch_timestamp) !== eligibleEpoch || rows.some((row, index) => typeof row.mark_fresh !== 'boolean' || index > 0 && (Number(row.epoch_timestamp) - Number(rows[index - 1].epoch_timestamp) !== 3600 || BigInt(row.block_number) <= BigInt(rows[index - 1].block_number)))) return { ready: false, reason: 'hourly_history_incomplete' }
    const first = rows[0], last = rows.at(-1)
    // Intermediate stale previews telescope through the accounting engine.
    // Only the reporting boundaries need fresh prices to define the return.
    if (!first.mark_fresh || !last.mark_fresh) return { ready: false, reason: 'stale_period_boundaries' }
    const startNumber = BigInt(first.block_number), endNumber = BigInt(last.block_number)
    if ((await this.block(startNumber, true)).hash !== first.block_hash || (await this.block(endNumber, true)).hash !== last.block_hash) throw new AttributionError('History boundary no longer canonical')
    // Index validation precedes reuse of a published report, so a reorg causes
    // repair even if an hourly boundary has not advanced yet.
    await this.index(endNumber)
    // The verified index tip commits to its entire block ancestry. Reorg repair
    // removes all evidence before reuse, so validated cached block identities
    // can be reused without hundreds of redundant archive block reads.
    const cachedSnapshots = await this.db.query("SELECT payload FROM vault_return_evidence WHERE deployment_key=$1 AND evidence_key LIKE 'snapshot:%'", [this.key])
    for (const { payload } of cachedSnapshots.rows) {
      const number = BigInt(payload.block)
      if (number >= startNumber && number <= endNumber && !this.blocks.has(number.toString())) this.blocks.set(number.toString(), {
        number, hash: payload.blockHash, timestamp: BigInt(payload.timestamp),
      })
    }
    const existing = await this.db.query('SELECT 1 FROM vault_return_reports WHERE chain_id=$1 AND house_pool_address=$2 AND start_block_hash=$3 AND end_block_hash=$4 AND start_share_price=$5 AND end_share_price=$6 AND schema_version=2 AND senior_vault_address=$7 AND junior_vault_address=$8 AND source_commit=$9', [this.release.network.chainId, this.address.housePool, first.block_hash, last.block_hash, first.junior_share_price_wad, last.junior_share_price_wad, this.address.seniorVault, this.address.juniorVault, SOURCE_COMMIT])
    if (existing.rows.length) return { ready: true, cached: true }
    const logs = (await this.db.query('SELECT payload FROM vault_return_logs WHERE deployment_key=$1 AND block_number <= $2 ORDER BY block_number,log_index', [this.key, endNumber.toString()])).rows.map(row => row.payload)
    const ledger = new EntryLedger()
    const events = new Map()
    const addEvent = (number, event) => {
      if (number <= startNumber) return
      const key = number.toString()
      events.set(key, [...events.get(key) ?? [], event])
    }
    const transactions = new Map()
    const capitalTransactions = new Map()
    for (const log of logs) {
      if (same(log.address, this.address.cfdEngine) && positionTopic(log.topics[0])) transactions.set(log.transactionHash, log)
      if (BigInt(log.blockNumber) > startNumber) {
        // Coupon checkpoints and fee changes can be triggered by vault request
        // flows too, so retain all relevant contract event boundaries.
        const key = BigInt(log.blockNumber).toString()
        if (!events.has(key)) events.set(key, [])
        const capital = capitalEvent(log, this.release)
        if (capital) {
          addEvent(BigInt(log.blockNumber), capital)
          capitalTransactions.set(log.transactionHash, log)
        }
        if (same(log.address, this.address.cfdEngine) && log.topics[0] === TOPICS.carry) addEvent(BigInt(log.blockNumber), { logIndex: Number(BigInt(log.logIndex)), sources: { realizedCarry: words(log.data, 4)[0] } })
      }
    }
    for (const [tx, entry] of transactions) {
      const evidence = await this.transactionEvidence(tx, entry.blockHash, BigInt(entry.blockNumber))
      for (const action of decodeActions(evidence.trace, evidence.logs, this.release)) {
        const sources = ledger.apply(action)
        addEvent(BigInt(entry.blockNumber), { logIndex: action.logIndex, sources })
      }
    }
    for (const [tx, entry] of capitalTransactions) {
      const evidence = await this.transactionEvidence(tx, entry.blockHash, BigInt(entry.blockNumber))
      const annotations = decodeLpValuations(evidence.trace, evidence.logs, this.release)
      const blockEvents = events.get(BigInt(entry.blockNumber).toString())
      for (const annotation of annotations) {
        const event = blockEvents.find(event => event.shareDelta !== undefined && event.logIndex === annotation.logIndex)
        if (!event) throw new AttributionError('LP valuation has no indexed capital event')
        event.terminalPnlBeforeFlow = annotation.terminalPnlBeforeFlow
      }
    }
    const checkpoints = new Set(rows.map(row => row.block_number.toString()))
    for (const block of events.keys()) {
      checkpoints.add(block)
      if (BigInt(block) - 1n > startNumber) checkpoints.add((BigInt(block) - 1n).toString())
    }
    const start = await this.snapshot(startNumber)
    if (start.sharePrice.toString() !== first.junior_share_price_wad) throw new AttributionError('Start valuation mismatch')
    const transitions = []
    for (const number of [...checkpoints].map(BigInt).filter(number => number > startNumber && number <= endNumber).sort((a, b) => a < b ? -1 : 1)) {
      transitions.push({ snapshot: await this.snapshot(number), events: (events.get(number.toString()) ?? []).sort((a, b) => a.logIndex - b.logIndex) })
      if (transitions.length % 100 === 0) this.log('info', 'vault_return_valuation_progress', { sampled: transitions.length, checkpoints: checkpoints.size })
    }
    const end = transitions.at(-1)?.snapshot
    if (!end || end.sharePrice.toString() !== last.junior_share_price_wad) throw new AttributionError('End valuation mismatch')
    const report = buildAttribution(start, end, transitions)
    report.sourceCommit = SOURCE_COMMIT
    report.sourceBlockCount = transitions.length + 1
    if ((await this.block(startNumber, true)).hash !== first.block_hash || (await this.block(endNumber, true)).hash !== last.block_hash) throw new AttributionError('Attribution reorged before publication')
    await this.db.query(`INSERT INTO vault_return_reports(chain_id,house_pool_address,senior_vault_address,junior_vault_address,start_block_number,end_block_number,start_block_hash,end_block_hash,start_share_price,end_share_price,source_commit,attribution,schema_version)
      VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12::jsonb,2)
      ON CONFLICT(chain_id,house_pool_address,senior_vault_address,junior_vault_address,end_block_number) DO UPDATE SET
      start_block_number=EXCLUDED.start_block_number,start_block_hash=EXCLUDED.start_block_hash,end_block_hash=EXCLUDED.end_block_hash,
      start_share_price=EXCLUDED.start_share_price,end_share_price=EXCLUDED.end_share_price,source_commit=EXCLUDED.source_commit,
      schema_version=2,attribution=EXCLUDED.attribution,updated_at=NOW()`,
    [this.release.network.chainId, this.address.housePool, this.address.seniorVault, this.address.juniorVault, startNumber.toString(), endNumber.toString(), first.block_hash, last.block_hash, start.sharePrice.toString(), end.sharePrice.toString(), SOURCE_COMMIT, json(report)])
    return { ready: true, cached: false, sourceBlockCount: transitions.length + 1, notes: report.notes }
  }
}
