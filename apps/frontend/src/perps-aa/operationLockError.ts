export class SponsoredOperationLockedError extends Error {
  readonly operationId: string

  constructor(operationId: string) {
    super('Another Trading Account action is already being submitted')
    this.name = 'SponsoredOperationLockedError'
    this.operationId = operationId
  }
}
