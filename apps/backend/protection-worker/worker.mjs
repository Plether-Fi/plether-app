import { encodeFunctionData, keccak256, isAddressEqual } from 'viem'
import abi from './abi.mjs'
import { admittedPayload, json, protectionSnapshot, retryDecision, triggerLeg } from './policy.mjs'

export class ProtectionWorker {
  constructor({ db, publicClient, walletClient, account, release, executionEnabled = false, log, batchBlocks = 2000n, confirmations = 12n, candidateBatch = 50, maxPayloadAge = 15, keeperBatchSize = 5, keeperPollSeconds = 5 }) {
    Object.assign(this, { db, publicClient, walletClient, account, release, executionEnabled, log, batchBlocks, confirmations, candidateBatch, maxPayloadAge, keeperBatchSize, keeperPollSeconds })
    this.chainId = release.network.chainId
    this.book = release.contracts.positionProtectionBook.address.toLowerCase()
    this.router = release.contracts.orderRouter.address
    this.lifecycle = release.contracts.orderLifecycleBook.address
    this.oracle = release.contracts.pletherOracle.address
    this.startBlock = BigInt(release.release.deploymentBlock)
    this.candidateCursor = 0n
  }

  read(address, contract, functionName, args = [], blockNumber) {
    return this.publicClient.readContract({ address, abi: abi[contract], functionName, args, blockNumber })
  }

  async verifyDeployment() {
    if (await this.publicClient.getChainId() !== this.chainId) throw new Error('Protection worker RPC chain does not match release')
    for (const name of ['positionProtectionBook', 'orderRouter', 'orderLifecycleBook', 'pletherOracle', 'cfdEngine']) {
      const contract = this.release.contracts[name]
      const code = await this.publicClient.getCode({ address: contract.address })
      if (!code || keccak256(code) !== contract.runtimeCodeHash.toLowerCase()) throw new Error(`Protection worker release bytecode mismatch: ${name}`)
    }
    const book = await this.read(this.router, 'OrderRouter', 'positionProtectionBook')
    const router = await this.read(this.book, 'PositionProtectionBook', 'ROUTER')
    const engine = await this.read(this.book, 'PositionProtectionBook', 'ENGINE')
    if (!isAddressEqual(book, this.book) || !isAddressEqual(router, this.router) || !isAddressEqual(engine, this.release.contracts.cfdEngine.address)) throw new Error('Protection Book binding mismatch')
    if (this.executionEnabled && !this.walletClient) throw new Error('Protection execution requires a dedicated signer')
  }

  async checkpoint() {
    return (await this.db.query('SELECT block_number,block_hash FROM perps_protection_checkpoints WHERE chain_id=$1 AND book=$2 ORDER BY block_number DESC LIMIT 1', [this.chainId, this.book])).rows[0]
  }

  async transaction(work) {
    await this.db.query('BEGIN')
    try { const result = await work(); await this.db.query('COMMIT'); return result }
    catch (error) { await this.db.query('ROLLBACK'); throw error }
  }

  async index() {
    let checkpoint = await this.checkpoint()
    // Walk durable range boundaries back to a canonical ancestor before replay.
    while (checkpoint) {
      const block = await this.publicClient.getBlock({ blockNumber: BigInt(checkpoint.block_number) })
      if (block.hash === checkpoint.block_hash) break
      const previous = (await this.db.query('SELECT block_number FROM perps_protection_checkpoints WHERE chain_id=$1 AND book=$2 AND block_number<$3 ORDER BY block_number DESC LIMIT 1', [this.chainId, this.book, checkpoint.block_number])).rows[0]
      const ancestor = previous ? BigInt(previous.block_number) : this.startBlock - 1n
      await this.transaction(async () => {
        await this.db.query('DELETE FROM perps_protection_events WHERE chain_id=$1 AND book=$2 AND block_number>$3', [this.chainId, this.book, ancestor.toString()])
        await this.db.query('DELETE FROM perps_protection_checkpoints WHERE chain_id=$1 AND book=$2 AND block_number>$3', [this.chainId, this.book, ancestor.toString()])
        await this.db.query('DELETE FROM perps_protection_observations WHERE chain_id=$1 AND book=$2 AND checked_block>$3', [this.chainId, this.book, ancestor.toString()])
      })
      this.log('warn', 'protection_reorg', { rewindBlock: ancestor.toString() })
      checkpoint = await this.checkpoint()
    }
    const latest = await this.publicClient.getBlock({ blockTag: 'latest' })
    const safe = { number: latest.number > this.confirmations ? latest.number - this.confirmations : 0n }
    const from = checkpoint ? BigInt(checkpoint.block_number) + 1n : this.startBlock
    if (from > safe.number) return { caughtUp: true, lagBlocks: 0n }
    const to = from + this.batchBlocks - 1n < safe.number ? from + this.batchBlocks - 1n : safe.number
    const boundary = await this.publicClient.getBlock({ blockNumber: to })
    const logs = await this.publicClient.getLogs({ address: this.book, events: abi.PositionProtectionBook.filter(item => item.type === 'event' && item.name.startsWith('PositionProtection')), fromBlock: from, toBlock: to, strict: true })
    logs.sort((a, b) => a.blockNumber < b.blockNumber ? -1 : a.blockNumber > b.blockNumber ? 1 : a.logIndex - b.logIndex)
    const snapshots = new Map()
    const blocks = new Map()
    for (const log of logs) {
      if (log.removed || log.blockNumber < from || log.blockNumber > to || !isAddressEqual(log.address, this.book)) throw new Error('Protection log scope mismatch')
      const number = log.blockNumber.toString()
      if (!blocks.has(number)) blocks.set(number, await this.publicClient.getBlock({ blockNumber: log.blockNumber }))
      if (blocks.get(number).hash !== log.blockHash) throw new Error('Protection log block hash mismatch')
      const key = `${number}:${log.args.protectionId}`
      if (!snapshots.has(key)) snapshots.set(key, await this.read(this.book, 'PositionProtectionBook', 'getPositionProtection', [log.args.protectionId], log.blockNumber))
      const state = snapshots.get(key)
      if (state.protectionId !== log.args.protectionId || !isAddressEqual(state.account, log.args.account)) throw new Error('Protection snapshot identity mismatch')
    }
    if ((await this.publicClient.getBlock({ blockNumber: to })).hash !== boundary.hash) throw new Error('Protection range changed during indexing')
    await this.transaction(async () => {
      for (const log of logs) {
        await this.db.query('INSERT INTO perps_protection_events(chain_id,book,protection_id,account,block_number,block_hash,log_index,transaction_hash,event_name,event_data,snapshot) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11) ON CONFLICT DO NOTHING', [this.chainId, this.book, log.args.protectionId.toString(), log.args.account.toLowerCase(), log.blockNumber.toString(), log.blockHash, log.logIndex, log.transactionHash, log.eventName, json(log.args), json(protectionSnapshot(snapshots.get(`${log.blockNumber}:${log.args.protectionId}`)))])
      }
      await this.db.query('INSERT INTO perps_protection_checkpoints(chain_id,book,block_number,block_hash) VALUES($1,$2,$3,$4)', [this.chainId, this.book, to.toString(), boundary.hash])
    })
    return { caughtUp: to === safe.number, lagBlocks: safe.number - to }
  }

  async reconcileTransaction() {
    const rows = (await this.db.query("SELECT * FROM perps_protection_transactions WHERE chain_id=$1 AND book=$2 AND status IN ('pending','included') ORDER BY created_at", [this.chainId, this.book])).rows
    for (const row of rows) {
    let receipt
    try { receipt = await this.publicClient.getTransactionReceipt({ hash: row.transaction_hash }) }
    catch (error) { if (error.name !== 'TransactionReceiptNotFoundError') throw error }
    let rebroadcast = !receipt
    if (receipt) {
      const [safe, canonical, latest] = await Promise.all([this.publicClient.getBlock({ blockTag: 'safe' }), this.publicClient.getBlock({ blockNumber: receipt.blockNumber }), this.publicClient.getBlock({ blockTag: 'latest' })])
      if (canonical.hash === receipt.blockHash && safe.number >= receipt.blockNumber) {
        await this.db.query('UPDATE perps_protection_transactions SET status=$1 WHERE chain_id=$2 AND book=$3 AND transaction_hash=$4', [receipt.status === 'success' ? 'confirmed' : 'reverted', this.chainId, this.book, row.transaction_hash])
        this.log(receipt.status === 'success' ? 'info' : 'error', 'protection_transaction_terminal', { hash: row.transaction_hash, status: receipt.status, protectionId: row.protection_id })
        continue
      }
      if (canonical.hash === receipt.blockHash && latest.number >= receipt.blockNumber + this.confirmations) {
        // Release the submission lane after confirmations, but retain exact-byte
        // reconciliation until safe finality so a deeper reorg is recoverable.
        await this.db.query("UPDATE perps_protection_transactions SET status='included' WHERE chain_id=$1 AND book=$2 AND transaction_hash=$3", [this.chainId, this.book, row.transaction_hash])
        continue
      }
      rebroadcast = canonical.hash !== receipt.blockHash
    }
    if (rebroadcast && this.executionEnabled) {
      // Exact signed bytes, including the nonce, survive process restarts. Never rebuild an ambiguous submission.
      try { await this.publicClient.sendRawTransaction({ serializedTransaction: row.raw_transaction }) }
      catch { this.log('warn', 'protection_rebroadcast_pending', { hash: row.transaction_hash }) }
    }
    if (Date.now() - new Date(row.created_at).getTime() > 120_000) this.log('error', 'protection_transaction_stalled', { hash: row.transaction_hash })
    return true
    }
    return false
  }

  async submit(action, protectionId, address, contract, functionName, args, value = 0n) {
    await this.publicClient.simulateContract({ account: this.account, address, abi: abi[contract], functionName, args, value })
    if (!this.executionEnabled) { this.log('info', 'protection_would_submit', { action, protectionId: protectionId.toString() }); return false }
    const request = await this.walletClient.prepareTransactionRequest({ account: this.account, to: address, data: encodeFunctionData({ abi: abi[contract], functionName, args }), value })
    request.gas = request.gas * 125n / 100n
    const raw = await this.walletClient.signTransaction(request)
    const hash = keccak256(raw)
    await this.db.query(`INSERT INTO perps_protection_transactions(chain_id,book,transaction_hash,raw_transaction,protection_id,action,linked_order_id)
      VALUES($1,$2,$3,$4,$5,$6,(SELECT (observation->>'linkedOrderId')::numeric FROM perps_protection_observations WHERE chain_id=$1 AND book=$2 AND protection_id=$5))`, [this.chainId, this.book, hash, raw, protectionId.toString(), action])
    // If the network response is lost, the durable row prevents a new nonce/action.
    await this.publicClient.sendRawTransaction({ serializedTransaction: raw })
    this.log('info', 'protection_submitted', { action, protectionId: protectionId.toString(), hash })
    return true
  }

  async observe(block, protection, reason, details = {}) {
    // This is a last-check explanation, not a promise of execution or canonical history.
    if ((await this.publicClient.getBlock({ blockNumber: block.number })).hash !== block.hash) throw new Error('Protection observation block changed')
    const observation = {
      protectionId: protection.protectionId.toString(), account: protection.account.toLowerCase(),
      linkedOrderId: protection.linkedOrderId.toString(), protectionStatus: protection.status,
      reason, ...details,
    }
    await this.db.query(`INSERT INTO perps_protection_observations(chain_id,book,protection_id,checked_block,checked_block_hash,observation)
      VALUES($1,$2,$3,$4,$5,$6) ON CONFLICT(chain_id,book,protection_id) DO UPDATE SET
      checked_block=EXCLUDED.checked_block,checked_block_hash=EXCLUDED.checked_block_hash,checked_at=NOW(),observation=EXCLUDED.observation`,
    [this.chainId, this.book, protection.protectionId.toString(), block.number.toString(), block.hash, json(observation)])
  }

  async evaluate() {
    if (await this.reconcileTransaction()) return
    const block = await this.publicClient.getBlock({ blockTag: 'latest' })
    const candidates = (await this.db.query("SELECT protection_id FROM (SELECT DISTINCT ON (protection_id) protection_id,snapshot FROM perps_protection_events WHERE chain_id=$1 AND book=$2 ORDER BY protection_id,block_number DESC,log_index DESC) latest WHERE (snapshot->>'status')::int IN (2,8) ORDER BY (protection_id>$3) DESC,protection_id LIMIT $4", [this.chainId, this.book, this.candidateCursor.toString(), this.candidateBatch])).rows
    if (!candidates.length) return
    const payload = (await this.db.query("SELECT source,min_publish_time,max_publish_time,update_data FROM perps_pyth_update_payloads WHERE source='backend_hermes_latest_v2' ORDER BY max_publish_time DESC LIMIT 1")).rows[0]
    const freshPayload = admittedPayload(payload, Number(block.timestamp), this.maxPayloadAge)
    const frozen = await this.read(this.oracle, 'PletherOracle', 'isOracleFrozen', [], block.number)
    let snapshot
    let fee = 0n
    if (freshPayload) {
      fee = await this.read(this.oracle, 'PletherOracle', 'getUpdateFee', [payload.update_data], block.number)
      try {
        // Simulate the same neutral MarkRefresh policy against the exact admitted Pyth bytes.
        snapshot = (await this.publicClient.simulateContract({ account: this.account, address: this.oracle, abi: abi.PletherOracle, functionName: 'updatePrice', args: [this.account.address, payload.update_data, 1], value: fee, blockNumber: block.number })).result
      } catch { this.log('warn', 'protection_oracle_unavailable', {}) }
    }
    if (!freshPayload && !frozen) this.log('warn', 'protection_oracle_unavailable', { reason: 'no-admitted-fresh-payload' })
    for (const candidate of candidates) {
      const id = BigInt(candidate.protection_id)
      this.candidateCursor = id
      let protection
      try {
        protection = await this.read(this.book, 'PositionProtectionBook', 'getPositionProtection', [id], block.number)
        if (![2, 8].includes(protection.status)) { await this.observe(block, protection, 'inactive'); continue }
        if (!this.executionEnabled) { await this.observe(block, protection, 'execution-disabled'); continue }
        if (!frozen && snapshot && triggerLeg(protection, snapshot.markPrice, snapshot.publishTime, block.number)) {
          await this.observe(block, protection, 'trigger-ready')
          if (await this.submit('trigger', id, this.book, 'PositionProtectionBook', 'triggerPositionProtection', [id, payload.update_data], fee)) return
        } else if (protection.status === 8) {
          const outcome = await this.read(this.lifecycle, 'OrderLifecycleBook', 'outcome', [protection.linkedOrderId], block.number)
          if (outcome.reason !== 2 || outcome.status !== 3) {
            await this.observe(block, protection, 'operator-required', { outcomeReason: outcome.reason })
            this.log('error', 'protection_operator_required', { protectionId: id.toString(), linkedOrderId: protection.linkedOrderId.toString(), reason: outcome.reason, failureSelector: outcome.failureSelector, revertDataHash: outcome.revertDataHash }); continue
          }
          const head = await this.read(this.router, 'OrderRouter', 'nextExecuteId', [], block.number)
          const tail = await this.read(this.router, 'OrderRouter', 'globalTailOrderId', [], block.number)
          if (tail !== 0n && head === tail) {
            const policy = await this.read(this.lifecycle, 'OrderLifecycleBook', 'pendingPolicy', [head], block.number)
            if (policy.validUntil > 0n && block.timestamp > policy.validUntil) {
              await this.observe(block, protection, 'queue-cleanup', { outcomeReason: outcome.reason })
              if (await this.submit('prune', id, this.router, 'OrderRouter', 'executeOrder', [head, []])) return
              continue // Re-read protection and receipt after the separate cleanup transaction.
            }
          }
          const maxOrderAge = await this.read(this.router, 'OrderRouter', 'maxOrderAge', [], block.number)
          // IDs include cancelled gaps. The span is a conservative upper bound on the live FIFO size.
          const queueSize = tail === 0n ? 0n : tail - head + 1n
          const pendingCount = await this.read(this.router, 'OrderRouter', 'pendingOrderCounts', [protection.account], block.number)
          const decision = retryDecision({ protection, outcome, pendingCount, oracleAvailable: Boolean(snapshot) || frozen, queueSize, maxOrderAge, keeperBatchSize: this.keeperBatchSize, keeperPollSeconds: this.keeperPollSeconds })
          await this.observe(block, protection, decision === 'retry' ? 'retry-ready' : decision, { outcomeReason: outcome.reason })
          if (decision === 'retry') {
            if (await this.submit('retry', id, this.book, 'PositionProtectionBook', 'retryPositionProtectionClose', [id])) return
          } else this.log('warn', 'protection_retry_waiting', { protectionId: id.toString(), reason: decision })
        } else await this.observe(block, protection, frozen ? 'oracle-frozen' : snapshot ? 'monitoring' : 'oracle-unavailable')
      } catch (error) {
        if (protection) await this.observe(block, protection, 'check-failed')
        this.log('warn', 'protection_simulation_failed', { protectionId: id.toString(), error: error.name })
        // A send failure may already have a durable transaction. Do not proceed to another candidate.
        if (await this.reconcileTransaction()) return
      }
    }
  }
}
