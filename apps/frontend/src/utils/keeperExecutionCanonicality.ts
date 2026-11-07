import type { Hex } from 'viem'

/** Only an explicit canonical hash replacement permits retracting a fast result. */
export class KeeperExecutionReorgedError extends Error {
  readonly transactionHash: Hex
  constructor(transactionHash: Hex) {
    super('The keeper transaction block was replaced; waiting for canonical execution.')
    this.name = 'KeeperExecutionReorgedError'
    this.transactionHash = transactionHash
  }
}
